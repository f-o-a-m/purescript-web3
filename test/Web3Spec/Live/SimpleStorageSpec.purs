module Web3Spec.Live.SimpleStorageSpec (spec) where

import Prelude

import Data.Either (isRight)
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Ethereum.Web3 (ChainCursor(..), Provider, _from, _to, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.SimpleStorage as SimpleStorage
import Web3Spec.LiveSpec.Utils (assertWeb3, defaultTestTxOptions, deployContract, mkUIntN, takeEvent)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "Simple Storage" $
    beforeAll ( deployContract provider "SimpleStorage" $ \txOpts ->
                  SimpleStorage.constructor txOpts SimpleStorage.deployBytecode
              ) $
      it "Can get and set a simple UInt with events" $ \simpleStorageCfg -> do
        let {contractAddress: simpleStorageAddress, userAddress} = simpleStorageCfg
            newCount = mkUIntN s256 1
            txOpts = defaultTestTxOptions # _from ?~ userAddress
                                          # _to ?~ simpleStorageAddress
            setCountTx = SimpleStorage.setCount txOpts {_count: newCount}
        Tuple _ (SimpleStorage.CountSet {_count}) <- assertWeb3 provider $
          takeEvent (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress setCountTx
        _count `shouldEqual` newCount
        eRes' <- runWeb3 provider $ Api.eth_getStorageAt simpleStorageAddress zero Latest
        eRes' `shouldSatisfy` isRight
