module Web3Spec.Live.FilterSpec (spec) where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Array (snoc)
import Data.Either (Either)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (traverse_)
import Data.Lens ((?~), (.~), (^.))
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
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
import Web3Spec.Live.Utils (assertWeb3, go, Logger, defaultTestTxOptions, ContractConfig, deployContract, mkUIntN, awaitNextBlock, joinWeb3Fork, hangOutTillBlock)


spec :: Provider -> SpecT Aff Unit Aff Unit
spec p =
  let env = { logger: \s -> ask >>= \logger -> liftAff $ logger s
            } 
  in go $ parallel $ spec' p env

type FilterEnv m =
  { logger :: String -> m Unit 
  }

spec' 
  :: forall m. 
     MonadAff m
  => Provider
  -> FilterEnv m
  -> SpecT m Unit Aff Unit
spec' provider {logger} =
  describe "Filters" do
      before (deployUniqueSimpleStorage provider logger) $
        it "can stream events starting and ending in the past" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              values = mkUIntN s256 <$> [1,2,3]
              filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
          fiber <- monitorUntil provider logger filter (_ == mkUIntN s256 3)
          start <- assertWeb3 provider Api.eth_blockNumber
          traverse_ setter values
          {endingBlockV} <- joinWeb3Fork fiber
          end <- liftAff $ AVar.take endingBlockV
          let pastFilter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                             # _fromBlock .~ BN start
                             # _toBlock .~  BN end
          fiber' <- monitorUntil provider logger pastFilter (const false)
          {foundValuesV} <- joinWeb3Fork fiber'
          foundValues <- liftAff $ AVar.take foundValuesV
          liftAff $ foundValues `shouldEqual` values

      before (deployUniqueSimpleStorage provider logger) $
        it "can stream events starting in the past and ending in the future" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              firstValues = mkUIntN s256 <$> [1,2,3]
              secondValues = mkUIntN s256 <$> [4,5,6]
              filter1 = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
          fiber1 <- monitorUntil provider logger filter1 (_ == mkUIntN s256 3)
          start <- assertWeb3 provider Api.eth_blockNumber
          traverse_ setter firstValues
          _ <- joinWeb3Fork fiber1
          let filter2 = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                           # _fromBlock .~ BN start
                           # _toBlock   .~ Latest
          fiber2 <- monitorUntil provider logger filter2 (_ == mkUIntN s256 6)
          traverse_ setter secondValues
          {foundValuesV} <- joinWeb3Fork fiber2
          foundValues <- liftAff $ AVar.take foundValuesV
          liftAff $ (firstValues <> secondValues) `shouldEqual` foundValues

      before (deployUniqueSimpleStorage provider logger) $
        it "can stream events starting and ending in the future, unbounded" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              values = mkUIntN s256 <$> [1,2,3]
          now <- assertWeb3 provider Api.eth_blockNumber
          let later = wrap $ unwrap now + embed 3
              filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                           # _fromBlock .~ BN later
                           # _toBlock   .~ Latest
          fiber <- monitorUntil provider logger filter (_ == mkUIntN s256 3)
          hangOutTillBlock provider logger later
          traverse_ setter values
          {foundValuesV} <- joinWeb3Fork fiber
          foundValues <- liftAff $ AVar.take foundValuesV
          liftAff $ foundValues `shouldEqual` values

      before (deployUniqueSimpleStorage provider logger) $
        it "can stream events starting and ending in the future, bounded" \simpleStorageCfg -> do
          let {simpleStorageAddress, setter} = simpleStorageCfg
              values = mkUIntN s256 <$> [8,9,10]
          now <- assertWeb3 provider Api.eth_blockNumber
          let later = wrap $ unwrap now + embed 3
              latest = wrap $ unwrap now + embed 8
              filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                         # _fromBlock .~ BN later
                         # _toBlock   .~ BN latest
          fiber <- monitorUntil provider logger filter (_ == mkUIntN s256 3)
          hangOutTillBlock provider logger later
          traverse_ setter values
          {foundValuesV} <- joinWeb3Fork fiber
          foundValues <- liftAff $ AVar.take foundValuesV
          liftAff $ foundValues `shouldEqual` values



--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

monitorUntil
  :: forall m.
     MonadAff m
  => Provider
  -> Logger m
  -> Filter SimpleStorage.CountSet
  -> (UIntN S256 -> Boolean)
  -> m
       ( Fiber
         ( Either Web3Error
             { endingBlockV :: AVar.AVar BlockNumber
             , foundValuesV :: AVar.AVar (Array (UIntN S256))
             }
         )
       )
monitorUntil provider logger filter p = do
  endingBlockV <- liftAff AVar.empty
  foundValuesV <- liftAff $ AVar.new []
  logger $ "Creating filter with fromBlock=" <> 
    show (filter ^. _fromBlock) <> " toBlock=" <> show (filter ^. _toBlock)
  liftAff $ forkWeb3 provider $ do
    event filter \(SimpleStorage.CountSet {_count}) -> do
      Change c <- ask
      foundSoFar <- liftAff $ AVar.take foundValuesV
      liftAff $ AVar.put (foundSoFar `snoc` _count) foundValuesV
      if p _count
        then do
          liftAff $ AVar.put c.blockNumber endingBlockV
          pure TerminateEvent
        else pure ContinueEvent
    pure {endingBlockV, foundValuesV}

deployUniqueSimpleStorage
  :: forall m.
     MonadAff m
  => Provider
  -> Logger m
  -> m { simpleStorageAddress :: Address
       , setter :: UIntN S256 -> m Unit
       } 
deployUniqueSimpleStorage provider logger = do
  contractConfig <- deployContract provider logger "SimpleStorage" $ \txOpts ->
    SimpleStorage.constructor txOpts SimpleStorageCode.deployBytecode
  pure { simpleStorageAddress: contractConfig.contractAddress
       , setter: mkSetter contractConfig provider logger
       }

mkSetter
  :: forall m.
     MonadAff m
  => ContractConfig
  -> Provider
  -> Logger m
  -> UIntN S256
  -> m Unit
mkSetter {contractAddress, userAddress} provider logger _count = do
  let txOptions = defaultTestTxOptions # _from ?~ userAddress
                                       # _to ?~ contractAddress
  logger $ "Setting count to " <> show _count
  _ <- assertWeb3 provider $ SimpleStorage.setCount txOptions {_count}
  awaitNextBlock provider logger