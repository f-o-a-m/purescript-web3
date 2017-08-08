module Web3Spec.Encoding.String (encodingStringSpec) where


import Prelude
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding (toDataBuilder, fromData)
import Network.Ethereum.Web3.Encoding.Bytes ()


encodingStringSpec :: forall r . Spec r Unit
encodingStringSpec = describe "encoding-spec" do

    describe "string tests" do

      it "can encode simple strings" do
         let given = "gavofyork"
         let expected =  HexString $ "0000000000000000000000000000000000000000000000000000000000000009"
                                  <> "6761766f66796f726b0000000000000000000000000000000000000000000000"
         expected `shouldEqual` toDataBuilder given
         fromData expected `shouldEqual` Just given

      it "can encode complicated strings" do
        let given = "welcome to ethereum. welcome to ethereum. welcome to ethereum."
        let expected = HexString $ "000000000000000000000000000000000000000000000000000000000000003e"
                                <> "77656c636f6d6520746f20657468657265756d2e2077656c636f6d6520746f20"
                                <> "657468657265756d2e2077656c636f6d6520746f20657468657265756d2e0000"

        expected `shouldEqual` toDataBuilder given
        fromData expected `shouldEqual` Just given
