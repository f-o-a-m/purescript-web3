module Web3Spec.Live.SimpleStorage (spec) where

import Prelude

import Data.Array ((!!))
import Data.Either (isRight)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Address, ChainCursor(..), EventAction(..), Provider, TransactionReceipt(..), _from, _to, event, eventFilter, forkWeb3, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafeCrashWith, unsafePartialBecause)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.SimpleStorage as SimpleStorage
import Web3Spec.LiveSpec.Utils (assertWeb3, defaultTestTxOptions, pollTransactionReceipt)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "It should be able to deploy and test a simple contract" $
    beforeAll (deploySimpleStorage provider) $

      it "Can deploy a contract, verify the contract storage, make a transaction, get get the event, make a call" $ \simpleStorageCfg -> do
        let {simpleStorageAddress, userAddress} = simpleStorageCfg
            newCount = unsafePartialBecause "one is a UINT" $ fromJust (uIntNFromBigNumber s256 one)
            fltr = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
        eventVar <-AVar.empty
        _ <- forkWeb3 provider $ event fltr \(SimpleStorage.CountSet {_count}) -> liftAff do
          liftEffect $ C.log $ "New Count Set: " <> show _count
          AVar.put _count eventVar
          pure TerminateEvent
        _ <- runWeb3 provider do
          let txOpts = defaultTestTxOptions # _from ?~ userAddress
                                            # _to ?~ simpleStorageAddress
          setCountHash <- SimpleStorage.setCount txOpts {_count: newCount}
          liftEffect $ C.log $ "Sumbitted count update transaction: " <> show setCountHash
        n <- AVar.take eventVar
        n `shouldEqual` newCount
        eRes' <- runWeb3 provider $ Api.eth_getStorageAt simpleStorageAddress zero Latest
        eRes' `shouldSatisfy` isRight

--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------

type SimpleStorageConfig =
  { simpleStorageAddress :: Address
  , userAddress :: Address
  }

deploySimpleStorage :: Provider -> Aff SimpleStorageConfig
deploySimpleStorage p = do
  userAddress <- assertWeb3 p $ do
    accounts <- Api.eth_getAccounts
    pure $ unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
  txHash <- assertWeb3 p do
    accounts <- Api.eth_getAccounts
    let txOpts = defaultTestTxOptions # _from ?~ userAddress
    txHash <- SimpleStorage.constructor txOpts SimpleStorage.deployBytecode
    liftEffect $ C.log $ "Submitted SimpleStorage deployment: " <> show txHash
    pure txHash
  let k (TransactionReceipt rec) = case rec.contractAddress of
        Nothing -> unsafeCrashWith "Contract deployment missing contractAddress in receipt"
        Just addr -> pure addr
  simpleStorageAddress <- pollTransactionReceipt txHash p k
  pure $ {simpleStorageAddress, userAddress}
