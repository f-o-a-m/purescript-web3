module Web3Spec.Live.FilterSpec (spec) where
  
import Prelude

import Control.Monad.Reader (ask)
import Data.Array (snoc)
import Data.Either (isRight, Either)
import Data.Traversable (traverse_)
import Data.Lens ((?~), (.~), (^.))
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (liftAff)
import Effect.Aff.AVar as AVar
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Web3, BlockNumber, Filter, Web3Error, Change(..), _fromBlock, _toBlock, eventFilter, EventAction(..), forkWeb3, event, ChainCursor(..), Provider, UIntN, _from, _to)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.Sizes (s256, S256)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.SimpleStorage as SimpleStorage
import Web3Spec.Live.Code.SimpleStorage as SimpleStorageCode
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions, ContractConfig, deployContract, mkUIntN, awaitNextBlock, joinWeb3Fork)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Filters" $
    beforeAll ( do
      contractConfig <- deployContract provider "SimpleStorage" $ \txOpts ->
        SimpleStorage.constructor txOpts SimpleStorageCode.deployBytecode
      pure { simpleStorageAddress: contractConfig.contractAddress
           , setter: mkSetter contractConfig
           }
    ) $

      it "can stream events starting and ending in the past"  $ \simpleStorageCfg -> do
        let {simpleStorageAddress, setter} = simpleStorageCfg
            values = mkUIntN s256 <$> [1,2,3]
        assertWeb3 provider awaitNextBlock
        let filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
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