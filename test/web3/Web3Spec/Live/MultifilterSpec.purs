module Web3Spec.Live.MultifilterSpec (spec) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask, lift)
import Control.Parallel (parSequence_, parTraverse_)
import Data.Array (elem, foldl, head, last, length, snoc, sort, unsnoc, (..))
import Data.Bifunctor (rmap)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (BigNumber, BlockNumber(..), Change(..), EventAction(..), EventHandler, Provider, Value, Web3, Wei, _data, _from, _to, _value, embed, event, event', eventFilter, forkWeb3, mkValue)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Test.Spec (SpecT, describe, it, beforeAll)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Code.Multifilter as MultifilterCode
import Web3Spec.Live.Contract.Multifilter as Multifilter
import Web3Spec.Live.Utils (deployContract, defaultTestTxOptions, mkUIntN, assertWeb3, joinWeb3Fork, awaitNextBlock, hangOutTillBlock)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Multifilter" do
    let pE1 = Proxy :: Proxy Multifilter.E1
        pE2 = Proxy :: Proxy Multifilter.E2
        fireE1 txOpts n = void $ assertWeb3 provider $ Multifilter.fireE1 txOpts {_value : mkUIntN s256 n}
        fireE2 txOpts n = void $ assertWeb3 provider $ Multifilter.fireE2 txOpts {_value: mkUIntN s256 n}
        logger = (liftAff <<< C.log)
    beforeAll ( deployContract provider C.log "Multifilter" $ \txOpts ->
                  Api.eth_sendTransaction $ txOpts # _data ?~ MultifilterCode.deployBytecode
                                                   # _value ?~ (mkValue zero :: Value Wei)
              ) $ do
      it "can receive multiple events in the correct order" \contractCfg -> do
        let {contractAddress: multifilterAddress, userAddress} = contractCfg
            txOpts = defaultTestTxOptions
                       # _from ?~ userAddress
                       # _to ?~ multifilterAddress
            vals1 = 1..5
            vals2 = 6..10
            nVals = length vals1 + length vals2

            filter1 = eventFilter pE1 multifilterAddress
            filter2 = eventFilter pE2 multifilterAddress

        raceV <- AVar.new []

        count1V <- AVar.new 0
        f1 <- forkWeb3 provider $ 
          let h1 = mkHandler pE1 provider raceV count1V (_ == length vals1) true
          in void $ event filter1 h1

        count2V <- AVar.new 0
        f2 <- forkWeb3 provider $ 
          let h2 = mkHandler pE2 provider raceV count2V (_ == length vals2) false
          in void $ event filter2 h2

        syncV <- AVar.new []
        sharedCountV <- AVar.new 0
        let multiFilter = { e1 : filter1, e2 : filter2}
            h1 = mkHandler pE1 provider syncV sharedCountV (_ == nVals) true
            h2 = mkHandler pE2 provider syncV sharedCountV (_ == nVals) false
            multiHandler = {e1: h1, e2: h2}
        f3 <- do
          f <- forkWeb3 provider $ event' multiFilter multiHandler {trailBy: 0, windowSize: 0}
          pure $ (rmap (const unit) <$> f)

        parSequence_ $
          [ parTraverse_ (fireE1 txOpts) vals1
          , parTraverse_ (fireE2 txOpts) vals2
          ]

        parTraverse_ joinWeb3Fork [f1, f2, f3]
        
        race <- AVar.take raceV
        sync <- AVar.take syncV
        
        race `shouldNotEqual` sync
        sort race `shouldEqual` sync
        sort sync `shouldEqual` sync

      it "optional multifilter hooks don't break type inference" \{contractAddress} -> do
        let dummyEvent'NoEvents1 = event' { } { } { trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents2 = event' { } { } { trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents3 = event' { } { } { trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit), afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents4 = event' { } { } { trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit), beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents5 = event' { } { } { afterFilterWindow: (\_ -> pure unit), trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents6 = event' { } { } { beforeFilterWindow: (\_ -> pure unit), trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents7 = event' { } { } { trailBy: 0, beforeFilterWindow: (\_ -> pure unit), windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'NoEvents8 = event' { } { } { trailBy: 0, afterFilterWindow: (\_ -> pure unit), windowSize: 0 }

            f1 = eventFilter pE1 contractAddress
            f2 = eventFilter pE2 contractAddress
            h1 = dummyHandler pE1 Nothing
            h2 = dummyHandler pE2 Nothing

            dummyEvent'OneEvent1 = event' { f1 } { f1: h1 } { trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent2 = event' { f1 } { f1: h1 } { trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent3 = event' { f1 } { f1: h1 } { trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit), afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent4 = event' { f1 } { f1: h1 } { trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit), beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent5 = event' { f1 } { f1: h1 } { afterFilterWindow: (\_ -> pure unit), trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent6 = event' { f1 } { f1: h1 } { beforeFilterWindow: (\_ -> pure unit), trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent7 = event' { f1 } { f1: h1 } { trailBy: 0, beforeFilterWindow: (\_ -> pure unit), windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'OneEvent8 = event' { f1 } { f1: h1 } { trailBy: 0, afterFilterWindow: (\_ -> pure unit), windowSize: 0 }

            dummyEvent'TwoEvents1 = event' { f1, f2 } { f1: h1, f2: h2 } { trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents2 = event' { f1, f2 } { f1: h1, f2: h2 } { trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents3 = event' { f1, f2 } { f1: h1, f2: h2 } { trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit), afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents4 = event' { f1, f2 } { f1: h1, f2: h2 } { trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit), beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents5 = event' { f1, f2 } { f1: h1, f2: h2 } { afterFilterWindow: (\_ -> pure unit), trailBy: 0, windowSize: 0, beforeFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents6 = event' { f1, f2 } { f1: h1, f2: h2 } { beforeFilterWindow: (\_ -> pure unit), trailBy: 0, windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents7 = event' { f1, f2 } { f1: h1, f2: h2 } { trailBy: 0, beforeFilterWindow: (\_ -> pure unit), windowSize: 0, afterFilterWindow: (\_ -> pure unit) }
            dummyEvent'TwoEvents8 = event' { f1, f2 } { f1: h1, f2: h2 } { trailBy: 0, afterFilterWindow: (\_ -> pure unit), windowSize: 0 }
        pure unit

      it "actually runs before and after hooks in proper order" \{contractAddress, userAddress} -> do
        (BlockNumber startBlock) <- assertWeb3 provider eth_blockNumber
        hooksAndEventsV <- AVar.new []
        counterV <- AVar.new 0
        let endBlock = startBlock + (embed 10)
            endBlock' = endBlock + (embed 3)
            storeFW when = storeHookValue hooksAndEventsV counterV (\{ counter, v } -> { counter, v: Left { when, v } })
            multiFilter = { e1: eventFilter pE1 contractAddress, e2: eventFilter pE2 contractAddress}
            evs = 1..5
            storeEV :: forall ev. Proxy ev -> EventHandler Web3 ev
            storeEV p = storeEventValue p hooksAndEventsV counterV (\{ counter, change } -> { counter, v: Right change }) (Just endBlock)
            multiHandler = { e1: storeEV pE1, e2: storeEV pE2 }
            txOpts = defaultTestTxOptions
                       # _from ?~ userAddress
                       # _to ?~ contractAddress

        f <- forkWeb3 provider $ event' multiFilter multiHandler
          { trailBy: 0
          , windowSize: 0
          , beforeFilterWindow: storeFW "before"
          , afterFilterWindow: storeFW "after"
          , onFilterTermination: \cr -> storeFW "filter_term" { start: cr.blockNumber, end: cr.blockNumber }
          }
        for_ evs $ \n -> fireE1 txOpts n *> fireE2 txOpts n *> awaitNextBlock provider logger
        hangOutTillBlock provider logger (BlockNumber endBlock)

        -- fire one more set of events so the filters terminate...
        fireE1 txOpts 99
        fireE2 txOpts 99

        void $ joinWeb3Fork f
        hooksAndEvents <- AVar.take hooksAndEventsV

        let evAsBeforeHook { counter,  v: Left { when: "before", v } } = Just { counter, v }
            evAsBeforeHook _ = Nothing

            evAsAfterHook { counter,  v: Left { when: "after", v } } = Just { counter, v }
            evAsAfterHook _ = Nothing

            evAsFilterTermHook { counter,  v: Left { when: "filter_term", v } } = Just { counter, v }
            evAsFilterTermHook _ = Nothing

            snoc' complete pending = if pending == [] then complete else snoc complete pending

            chainFolder st@{ complete, pending } ev = case ev of
              { v: Left { when: "before" } } -> { complete: snoc' complete pending, pending: [ev] }
              { v: Left { when: "after" } }  -> { complete: snoc complete (snoc pending ev), pending: [] }
              { v: Left { when: "filter_term" } } -> { complete: snoc complete (snoc pending ev), pending: [] }
              _ -> st { pending = snoc pending ev }
            chains = foldl chainFolder { complete: [], pending: [] } hooksAndEvents

        for_ chains.complete $ \chain -> do
          let start = evAsBeforeHook =<< head chain
              end = evAsAfterHook =<< last chain
              end' = evAsFilterTermHook =<< last chain
              consecFolder { prevBN, prevCounter, isConsecutiveCounter, isConsecutiveBN } curr =
                let { counter, bn } =
                      case curr of
                        { counter, v: Right (Change { blockNumber: (BlockNumber bn) }) } -> { counter, bn }
                        { counter, v: Left { when: "before", v: { start } } } -> { counter, bn: un BlockNumber start }
                        { counter, v: Left { when: "after", v: { end } } } -> { counter, bn: un BlockNumber end }
                        { counter } -> { counter, bn: embed 0 } -- ok to ignore when "filter_term" -- we drop it in `chainToFold`
                    consCounter' = isConsecutiveCounter && (counter >= prevCounter)
                    consBN' = isConsecutiveBN && (bn >= prevBN)
                 in { prevBN: bn, prevCounter: counter, isConsecutiveCounter: consCounter', isConsecutiveBN: consBN' }

              chainToFold = if isJust end' then fromMaybe [] (_.init <$> unsnoc chain) else chain
              evsConsecutive = foldl consecFolder { prevBN: embed 0, prevCounter: -1, isConsecutiveCounter: true, isConsecutiveBN: true } chainToFold

          start `shouldSatisfy` isJust

          if isNothing end'
          then end `shouldSatisfy` isJust
          else end' `shouldSatisfy` isJust

          evsConsecutive `shouldSatisfy` _.isConsecutiveCounter
          evsConsecutive `shouldSatisfy` _.isConsecutiveBN

        chains.pending `shouldEqual` []

dummyHandler :: forall e. Proxy e -> Maybe BigNumber -> EventHandler Web3 e
dummyHandler _ Nothing _ = pure TerminateEvent
dummyHandler _ (Just bn) _ = (\(BlockNumber x) -> if x >= bn then TerminateEvent else ContinueEvent) <$> lift eth_blockNumber

storeHookValue
  :: forall a e.
     AVar.AVar (Array e)
  -> AVar.AVar Int
  -> ({ counter :: Int, v :: a } -> e)
  -> a
  -> Web3 Unit
storeHookValue avar counterVar hook2e v = liftAff $ do
  counter <- do
    originalVal <- AVar.take counterVar
    AVar.put (originalVal + 1) counterVar
    pure originalVal
  av <- AVar.take avar
  AVar.put (snoc av (hook2e { counter, v })) avar

storeEventValue
  :: forall a e
   . Proxy a
  -> AVar.AVar (Array e)
  -> AVar.AVar Int
  -> ({ counter :: Int, change :: Change, ev :: a } -> e)
  -> Maybe BigNumber
  -> EventHandler Web3 a
storeEventValue pa avar counterVar ev2e terminateBlock ev = do
  change <- ask
  liftAff do
    counter <- do
      originalVal <- AVar.take counterVar
      AVar.put (originalVal + 1) counterVar
      pure originalVal
    av <- AVar.take avar
    AVar.put (snoc av (ev2e { counter, change, ev })) avar
  dummyHandler pa terminateBlock ev

mkHandler
  :: forall e.
     Proxy e
  -> Provider
  -> AVar.AVar (Array (Tuple BlockNumber BigNumber))
  -> AVar.AVar Int
  -> (Int -> Boolean)
  -> Boolean
  -> EventHandler Web3 e
mkHandler _ provider indexV countV p shouldDelay = \e -> do
  Change c <- ask
  iAcc <- liftAff $ AVar.take indexV
  let index = Tuple c.blockNumber c.logIndex
      indices = iAcc `snoc` index
  liftAff $ AVar.put indices indexV
  count <- liftAff $ AVar.take countV
  let count' = count + 1
  liftAff $ AVar.put count' countV
  if p count'
    then pure TerminateEvent
    else do
      when shouldDelay $ 
        awaitNextBlock provider C.log
      pure ContinueEvent