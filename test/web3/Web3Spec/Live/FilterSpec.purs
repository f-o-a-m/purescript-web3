module Web3Spec.Live.FilterSpec (spec) where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Array (snoc)
import Data.Either (Either)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse_)
import Data.Lens ((?~), (.~), (^.))
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (liftAff)
import Effect.Aff.AVar as AVar
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Web3, BlockNumber, Filter, Web3Error, Change(..), _fromBlock, _toBlock, eventFilter, EventAction(..), forkWeb3, event, ChainCursor(..), Provider, UIntN, _from, _to, embed, Address)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.Sizes (s256, S256)
import Test.Spec (SpecT, before, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.SimpleStorage as SimpleStorage
import Web3Spec.Live.Code.SimpleStorage as SimpleStorageCode
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions, ContractConfig, deployContract, mkUIntN, awaitNextBlock, joinWeb3Fork, hangOutTillBlock)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Filters" $ parallel do
      before (deployUniqueSimpleStorage provider) $
        it "can stream events starting and ending in the past" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              values = mkUIntN s256 <$> [1,2,3]
              filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
          fiber <- monitorUntil provider filter (_ == mkUIntN s256 3)
          start <- assertWeb3 provider Api.eth_blockNumber
          assertWeb3 provider $ traverse_ setter values
          {endingBlockV} <- joinWeb3Fork fiber
          end <- AVar.take endingBlockV
          let pastFilter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                             # _fromBlock .~ BN start
                             # _toBlock .~  BN end
          fiber' <- monitorUntil provider pastFilter (const false)
          {foundValuesV} <- joinWeb3Fork fiber'
          foundValues <- AVar.take foundValuesV
          foundValues `shouldEqual` values

      before (deployUniqueSimpleStorage provider) $
        it "can stream events starting in the past and ending in the future" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              firstValues = mkUIntN s256 <$> [1,2,3]
              secondValues = mkUIntN s256 <$> [4,5,6]
              filter1 = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
          fiber1 <- monitorUntil provider filter1 (_ == mkUIntN s256 3)
          start <- assertWeb3 provider Api.eth_blockNumber
          assertWeb3 provider $ traverse_ setter firstValues
          _ <- joinWeb3Fork fiber1
          let filter2 = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                           # _fromBlock .~ BN start
                           # _toBlock   .~ Latest
          fiber2 <- monitorUntil provider filter2 (_ == mkUIntN s256 6)
          assertWeb3 provider $ traverse_ setter secondValues
          {foundValuesV} <- joinWeb3Fork fiber2
          foundValues <- AVar.take foundValuesV
          (firstValues <> secondValues) `shouldEqual` foundValues

      before (deployUniqueSimpleStorage provider) $
        it "can stream events starting and ending in the future, unbounded" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              values = mkUIntN s256 <$> [1,2,3]
          now <- assertWeb3 provider Api.eth_blockNumber
          let later = wrap $ unwrap now + embed 3
              filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                           # _fromBlock .~ BN later
                           # _toBlock   .~ Latest
          fiber <- monitorUntil provider filter (_ == mkUIntN s256 3)
          assertWeb3 provider $ hangOutTillBlock later
          assertWeb3 provider $ traverse_ setter values
          {foundValuesV} <- joinWeb3Fork fiber
          foundValues <- AVar.take foundValuesV
          foundValues `shouldEqual` values

      before (deployUniqueSimpleStorage provider) $
        it "can stream events starting and ending in the future, bounded" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              values = mkUIntN s256 <$> [8,9,10]
          now <- assertWeb3 provider Api.eth_blockNumber
          let later = wrap $ unwrap now + embed 3
              latest = wrap $ unwrap now + embed 8
              filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                         # _fromBlock .~ BN later
                         # _toBlock   .~ BN latest
          fiber <- monitorUntil provider filter (_ == mkUIntN s256 3)
          assertWeb3 provider $ hangOutTillBlock later
          assertWeb3 provider $ traverse_ setter values
          {foundValuesV} <- joinWeb3Fork fiber
          foundValues <- AVar.take foundValuesV
          foundValues `shouldEqual` values



--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

monitorUntil
  :: Provider
  -> Filter SimpleStorage.CountSet
  -> (UIntN S256 -> Boolean)
  -> Aff 
       ( Fiber
         ( Either Web3Error
             { endingBlockV :: AVar.AVar BlockNumber
             , foundValuesV :: AVar.AVar (Array (UIntN S256))
             }
         )
       )
monitorUntil provider filter p = do
  endingBlockV <- liftAff AVar.empty
  foundValuesV <- liftAff $ AVar.new []
  C.log $ "Creating filter with fromBlock=" <> 
    show (filter ^. _fromBlock) <> " toBlock=" <> show (filter ^. _toBlock)
  forkWeb3 provider $ do
    event filter \(SimpleStorage.CountSet {_count}) -> do
      Change c <- ask
      C.log $ "[ " <> show c.blockNumber <> " ]" <> " count set to " <> show _count
      foundSoFar <- liftAff $ AVar.take foundValuesV
      liftAff $ AVar.put (foundSoFar `snoc` _count) foundValuesV
      if p _count
        then do
          liftAff $ AVar.put c.blockNumber endingBlockV
          pure TerminateEvent
        else pure ContinueEvent
    pure {endingBlockV, foundValuesV}

deployUniqueSimpleStorage
  :: Provider
  -> Aff { simpleStorageAddress :: Address
         , setter :: UIntN S256 -> Web3 Unit
         } 
deployUniqueSimpleStorage provider = do
  contractConfig <- deployContract provider "SimpleStorage" $ \txOpts ->
    SimpleStorage.constructor txOpts SimpleStorageCode.deployBytecode
  pure { simpleStorageAddress: contractConfig.contractAddress
       , setter: mkSetter contractConfig
       }

mkSetter
  :: ContractConfig
  -> UIntN S256
  -> Web3 Unit
mkSetter {contractAddress, userAddress} _count = do
  let txOptions = defaultTestTxOptions # _from ?~ userAddress
                                       # _to ?~ contractAddress
  C.log $ "Setting count to " <> show _count
  _ <- SimpleStorage.setCount txOptions {_count}
  awaitNextBlock