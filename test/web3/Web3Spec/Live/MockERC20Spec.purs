module Web3Spec.Live.MockERC20Spec where

import Prelude

import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Provider, _from, _to, _value, mkValue, Value, Wei, _data)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.MockERC20 as MockERC20
import Web3Spec.Live.Code.MockERC20 as MockERC20Code
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions, deployContract, mkUIntN, takeEvent, nullAddress)


spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "MockERC20" $

    beforeAll ( deployContract provider C.log "MockERC20" $ \txOpts ->
                  Api.eth_sendTransaction $ txOpts # _data ?~ MockERC20Code.deployBytecode
                                                   # _value ?~ (mkValue zero :: Value Wei)
              ) $
      it "can make a transfer" $ \cfg -> do
        let {contractAddress: mockERC20Address, userAddress} = cfg
            amount = mkUIntN s256 1
            recipient = nullAddress
            txOptions = defaultTestTxOptions # _from ?~ userAddress
                                             # _to ?~ mockERC20Address
            transferAction = MockERC20.transfer txOptions {to : recipient, amount : amount}
        Tuple _ (MockERC20.Transfer tfr) <- assertWeb3 provider $ 
          takeEvent (Proxy :: Proxy MockERC20.Transfer) mockERC20Address transferAction
        tfr.amount `shouldEqual` amount