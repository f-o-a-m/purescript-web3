module Web3Spec.Live.MultifilterSpec (spec) where

import Prelude
import Control.Monad.Reader (ask)
import Data.Array (snoc, (..), length, sort)
import Data.Bifunctor (rmap)
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Control.Parallel (parSequence_, parTraverse_)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.AVar as AVar
import Effect.Class.Console as C
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3 (Provider, BlockNumber, EventHandler, Web3, event, event', BigNumber, forkWeb3, eventFilter, EventAction(..), Change(..), _data, _from, _to, _value, mkValue, Value, Wei)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Test.Spec (SpecT, describe, it, beforeAll)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Utils (deployContract, defaultTestTxOptions, mkUIntN, assertWeb3, joinWeb3Fork, awaitNextBlock)
import Web3Spec.Live.Code.Multifilter as MultifilterCode
import Web3Spec.Live.Contract.Multifilter as Multifilter

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Multifilter"
    $ beforeAll
        ( deployContract provider C.log "Multifilter"
            $ \txOpts ->
                Api.eth_sendTransaction $ txOpts # _data ?~ MultifilterCode.deployBytecode
                  # _value
                  ?~ (mkValue zero :: Value Wei)
        )
    $ it "can receive multiple events in the correct order" \contractCfg -> do
        let
          { contractAddress: multifilterAddress, userAddress } = contractCfg

          txOpts =
            defaultTestTxOptions
              # _from
              ?~ userAddress
              # _to
              ?~ multifilterAddress

          vals1 = 1 .. 5

          vals2 = 6 .. 10

          nVals = length vals1 + length vals2

          fireE1 n = void $ assertWeb3 provider $ Multifilter.fireE1 txOpts { _value: mkUIntN s256 n }

          fireE2 n = void $ assertWeb3 provider $ Multifilter.fireE2 txOpts { _value: mkUIntN s256 n }

          filter1 = eventFilter (Proxy :: Proxy Multifilter.E1) multifilterAddress

          filter2 = eventFilter (Proxy :: Proxy Multifilter.E2) multifilterAddress
        raceV <- AVar.new []
        count1V <- AVar.new 0
        f1 <-
          forkWeb3 provider
            $ let
                h1 = mkHandler (Proxy :: Proxy Multifilter.E1) provider raceV count1V (_ == length vals1) true
              in
                void $ event filter1 h1
        count2V <- AVar.new 0
        f2 <-
          forkWeb3 provider
            $ let
                h2 = mkHandler (Proxy :: Proxy Multifilter.E2) provider raceV count2V (_ == length vals2) false
              in
                void $ event filter2 h2
        syncV <- AVar.new []
        sharedCountV <- AVar.new 0
        let
          multiFilter = { e1: filter1, e2: filter2 }

          h1 = mkHandler (Proxy :: Proxy Multifilter.E1) provider syncV sharedCountV (_ == nVals) true

          h2 = mkHandler (Proxy :: Proxy Multifilter.E2) provider syncV sharedCountV (_ == nVals) false

          multiHandler = { e1: h1, e2: h2 }
        f3 <- do
          f <- forkWeb3 provider $ event' multiFilter multiHandler { trailBy: 0, windowSize: 0 }
          pure $ (rmap (const unit) <$> f)
        parSequence_
          $ [ parTraverse_ fireE1 vals1
            , parTraverse_ fireE2 vals2
            ]
        parTraverse_ joinWeb3Fork [ f1, f2, f3 ]
        race <- AVar.take raceV
        sync <- AVar.take syncV
        race `shouldNotEqual` sync
        sort race `shouldEqual` sync
        sort sync `shouldEqual` sync

mkHandler ::
  forall e.
  Proxy e ->
  Provider ->
  AVar.AVar (Array (Tuple BlockNumber BigNumber)) ->
  AVar.AVar Int ->
  (Int -> Boolean) ->
  Boolean ->
  EventHandler Web3 e
mkHandler _ provider indexV countV p shouldDelay = \_e -> do
  Change c <- ask
  iAcc <- liftAff $ AVar.take indexV
  let
    index = Tuple c.blockNumber c.logIndex

    indices = iAcc `snoc` index
  liftAff $ AVar.put indices indexV
  count <- liftAff $ AVar.take countV
  let
    count' = count + 1
  liftAff $ AVar.put count' countV
  if p count' then
    pure TerminateEvent
  else do
    when shouldDelay
      $ awaitNextBlock provider C.log
    pure ContinueEvent
