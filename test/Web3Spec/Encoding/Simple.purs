module Web3Spec.Encoding.Simple (encodingSimpleSpec) where


import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Test.Spec (Spec, describe, it)
import Data.ByteString as BS
import Partial.Unsafe (unsafePartial)
import Control.Monad.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)
import Network.Ethereum.Web3.Types (Address(..), HexString(..))
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder, fromData)
import Network.Ethereum.Web3.Encoding.Bytes(BytesN, fromByteString)
import Network.Ethereum.Web3.Encoding.Size(D1, D2, D3, type (:&))


encodingSimpleSpec :: forall r . Spec r Unit
encodingSimpleSpec = describe "encoding-spec" do
  stringTests
  bytesDTests
  bytesNTests
  intTests
  addressTests

roundTrip :: forall r a . Show a => Eq a => ABIEncoding a => a -> HexString -> Aff r Unit
roundTrip decoded encoded = do
  encoded `shouldEqual` toDataBuilder decoded
  fromData encoded `shouldEqual` Just decoded

stringTests :: forall r . Spec r Unit
stringTests =
    describe "string tests" do

      it "can encode simple strings" do
         let given = "gavofyork"
         let expected =  HexString $ "0000000000000000000000000000000000000000000000000000000000000009"
                                  <> "6761766f66796f726b0000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode complicated strings" do
        let given = "welcome to ethereum. welcome to ethereum. welcome to ethereum."
        let expected = HexString $ "000000000000000000000000000000000000000000000000000000000000003e"
                                <> "77656c636f6d6520746f20657468657265756d2e2077656c636f6d6520746f20"
                                <> "657468657265756d2e2077656c636f6d6520746f20657468657265756d2e0000"
        roundTrip given expected


      it "can encode unicode strings" do
        let given = "Ã¤Ã¤"
        let expected = HexString $ "0000000000000000000000000000000000000000000000000000000000000008"
                                <> "c383c2a4c383c2a4000000000000000000000000000000000000000000000000"

        roundTrip given expected

bytesDTests :: forall r . Spec r Unit
bytesDTests =
    describe "bytesD tests" do

      it "can encode short bytesD" do
         let given = flip BS.fromString BS.Hex $ "c3a40000c3a4"
         let expected = HexString $
                          "0000000000000000000000000000000000000000000000000000000000000006"
                       <> "c3a40000c3a40000000000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode long bytesD" do
         let given = flip BS.fromString BS.Hex $
                            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1"
         let expected = HexString $
                            "000000000000000000000000000000000000000000000000000000000000009f"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff100"
         roundTrip given expected

bytesNTests :: forall r . Spec r Unit
bytesNTests =
    describe "byteN tests" do

      it "can encode Bytes1" do
         let mgiven =  (fromByteString <<< flip BS.fromString BS.Hex $ "cf") :: Maybe (BytesN D1)
             given = unsafePartial $ fromJust mgiven
             expected =  HexString "cf00000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode Bytes3" do
         let mgiven =  (fromByteString <<< flip BS.fromString BS.Hex $ "cf0011") :: Maybe (BytesN D3)
             given = unsafePartial $ fromJust mgiven
             expected =  HexString "cf00110000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected


      it "can encode Bytes12" do
         let mgiven =  (fromByteString <<< flip BS.fromString BS.Hex $ "6761766f66796f726b000000") :: Maybe (BytesN (D1 :& D2))
             given = unsafePartial $ fromJust mgiven
             expected =  HexString "6761766f66796f726b0000000000000000000000000000000000000000000000"
         roundTrip given expected


intTests :: forall r . Spec r Unit
intTests =
    describe "int/uint tests" do

      it "can encode int" do
         let given = 21
         let expected =  HexString "0000000000000000000000000000000000000000000000000000000000000015"
         roundTrip given expected

      it "can encode negative numbers" do
         let given = negate 1
         let expected =  HexString "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip given expected


      it "can encode some big number" do
         let given = 987654321
         let expected =  HexString "000000000000000000000000000000000000000000000000000000003ade68b1"
         roundTrip given expected

addressTests :: forall r . Spec r Unit
addressTests =
    describe "addresses tests" do

      it "can encode address" do
         let given = Address <<< HexString $ "407d73d8a49eeb85d32cf465507dd71d507100c1"
         let expected =  HexString "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c1"
         roundTrip given expected
