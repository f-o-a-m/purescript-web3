module Web3Spec.Live.SimpleErrorTestSpec (spec) where

import Prelude
import Data.Either (Either(..), isLeft)
import Data.Lens ((?~))
import Effect.Aff (Aff)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (Provider, ChainCursor(..), _from, _to, _value, mkValue, Value, Wei, _data)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Api as Api
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Web3Spec.Live.Contract.SimpleErrorTest as SimpleErrorTest
import Web3Spec.Live.Code.SimpleErrorTest as SimpleErrorTestCode
import Web3Spec.Live.Utils (assertWeb3, defaultTestTxOptions, deployContract, mkUIntN)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
  describe "SimpleError"
    $ beforeAll
        ( deployContract provider C.log "SimpleErrorTest"
            $ \txOpts ->
                Api.eth_sendTransaction $ txOpts # _data ?~ SimpleErrorTestCode.deployBytecode
                  # _value
                      ?~ (mkValue zero :: Value Wei)
        )
    $ describe "SimpleError" do
        it "can raise a left for unset values"
          $ \cfg -> do
              let
                { contractAddress: simpleErrorTestAddress, userAddress } = cfg

                txOptions =
                  defaultTestTxOptions # _to ?~ simpleErrorTestAddress
                    # _from
                        ?~ userAddress

                n = mkUIntN s256 1
              resp1 <- assertWeb3 provider $ SimpleErrorTest.names txOptions Latest n
              resp1 `shouldSatisfy` isLeft
              resp2 <- assertWeb3 provider $ SimpleErrorTest.testBool txOptions Latest { _arg: true }
              resp2 `shouldEqual` Right false
              resp3 <- assertWeb3 provider $ SimpleErrorTest.testBool txOptions Latest { _arg: false }
              resp3 `shouldEqual` Right true
