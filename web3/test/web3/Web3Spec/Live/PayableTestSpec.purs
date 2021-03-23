module Web3Spec.Live.PayableTestSpec (spec) where

import Prelude
import Data.Lens ((?~))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Provider, _from, _to, _value, mkValue, Value, Wei, Ether, Shannon, _data, unUIntN, convert)
import Network.Ethereum.Web3.Api as Api
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.PayableTest as PayableTest
import Web3Spec.Live.Code.PayableTest as PayableTestCode
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions, deployContract, takeEvent)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "PayableTest"
    $ beforeAll
        ( deployContract provider C.log "PayableTest"
            $ \txOpts ->
                Api.eth_sendTransaction $ txOpts # _data ?~ PayableTestCode.deployBytecode
                  # _value
                  ?~ (mkValue zero :: Value Wei)
        )
    $ describe "PayableTest" do
        it "can send the right amount of Ether"
          $ \cfg -> do
              let
                { contractAddress: payableTestAddress, userAddress } = cfg

                txOptions =
                  defaultTestTxOptions # _to ?~ payableTestAddress
                    # _value
                    ?~ convert (mkValue one :: Value Ether)
                    # _from
                    ?~ userAddress

                etherAction = PayableTest.seeContent txOptions
              Tuple _ (PayableTest.Content c) <-
                assertWeb3 provider
                  $ takeEvent (Proxy :: Proxy PayableTest.Content) payableTestAddress etherAction
              unUIntN c._paidContent `shouldEqual` one
        it "can send the right amount of Shannon"
          $ \cfg -> do
              let
                { contractAddress: payableTestAddress, userAddress } = cfg

                txOptions =
                  defaultTestTxOptions # _to ?~ payableTestAddress
                    # _value
                    ?~ convert (mkValue one :: Value Shannon)
                    # _from
                    ?~ userAddress

                etherAction = PayableTest.seeContent txOptions
              Tuple _ (PayableTest.Content c) <-
                assertWeb3 provider
                  $ takeEvent (Proxy :: Proxy PayableTest.Content) payableTestAddress etherAction
              unUIntN c._paidContent `shouldEqual` zero
