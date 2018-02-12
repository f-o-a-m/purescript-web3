module Web3Spec.Encoding.Simple (encodingSimpleSpec) where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Data.Array (replicate)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (ForeignError)
import Data.Foreign.Generic (decodeJSON, defaultOptions)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (toLower)
import Data.Traversable (sequence)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncode, class ABIDecode, toDataBuilder, fromData)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, fromByteString)
import Network.Ethereum.Web3.Solidity.Int (IntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Size (D1, D2, D3, D4, D5, D6, D8, type (:&))
import Network.Ethereum.Web3.Solidity.UInt (UIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Types (FalseOrObject(..), HexString, SyncStatus(..), embed, mkAddress, mkHexString, pow, unHex)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)


encodingSimpleSpec :: forall r . Spec (console :: CONSOLE | r) Unit
encodingSimpleSpec = describe "encoding-spec" do
  stringTests
  bytesDTests
  bytesNTests
  intTests
  uintNTests
  intNTests
  addressTests
  falseOrObjectTests

roundTrip :: forall r a . Show a => Eq a => ABIEncode a => ABIDecode a => a -> HexString -> Aff r Unit
roundTrip decoded encoded = do
  encoded `shouldEqual` toDataBuilder decoded
  fromData encoded `shouldEqual` Right decoded

stringTests :: forall r . Spec r Unit
stringTests =
    describe "string tests" do

      it "can encode simple strings" do
         let given = "gavofyork"
         let expected =  unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000009"
                                  <> "6761766f66796f726b0000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode complicated strings" do
        let given = "welcome to ethereum. welcome to ethereum. welcome to ethereum."
        let expected = unsafePartial fromJust <<< mkHexString $ "000000000000000000000000000000000000000000000000000000000000003e"
                                <> "77656c636f6d6520746f20657468657265756d2e2077656c636f6d6520746f20"
                                <> "657468657265756d2e2077656c636f6d6520746f20657468657265756d2e0000"
        roundTrip given expected


      it "can encode unicode strings" do
        let given = "Ã¤Ã¤"
        let expected = unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000008"
                                <> "c383c2a4c383c2a4000000000000000000000000000000000000000000000000"

        roundTrip given expected


      it "can handle VERY long HexStrings" do
        let given = intercalate "" $ replicate 128 "0000000000000000000000000000000000000000000000000000000000000000"
        let expected = unsafePartial fromJust <<< mkHexString $ given
        given `shouldEqual` unHex expected

      it "can handle mixed case HexStrings" do
        let given = "fF"
        let expected = unsafePartial fromJust <<< mkHexString $ given
        -- note; for easy equality we should canonicalize HexStrings as lowercase
        toLower given `shouldEqual` unHex expected

      it "fails on odd length HexStrings" do
        let givens = ["f", "0", "000", "0f0", "fffff", "0000000000000000000000000000000000000000f"]
        _ <- sequence $ map (\g -> mkHexString g `shouldEqual` Nothing) givens
        pure unit

      it "should hold equality across cases" do
        mkHexString "ff" `shouldEqual` mkHexString "Ff"
        mkHexString "0000aa" `shouldEqual` mkHexString "0000AA"
        mkHexString "0000aa" `shouldEqual` mkHexString "0000aa"
        mkHexString "abcdef" `shouldEqual` mkHexString "AbCdEf"
        mkHexString "" `shouldNotEqual` mkHexString "ff"
        mkHexString "ff" `shouldNotEqual` mkHexString "aa"


bytesDTests :: forall r . Spec r Unit
bytesDTests =
    describe "bytesD tests" do

      it "can encode short bytesD" do
         let given = unsafePartial $ fromJust $flip BS.fromString BS.Hex $ "c3a40000c3a4"
         let expected = unsafePartial fromJust <<< mkHexString $
                          "0000000000000000000000000000000000000000000000000000000000000006"
                       <> "c3a40000c3a40000000000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode long bytesD" do
         let given = unsafePartial $ fromJust $ flip BS.fromString BS.Hex $
                            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1"
         let expected = unsafePartial fromJust <<< mkHexString $
                            "000000000000000000000000000000000000000000000000000000000000009f"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff100"
         roundTrip given expected

      it "can encode dave" do
        let given = "dave"
        let expected = unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000004"
                                <> "6461766500000000000000000000000000000000000000000000000000000000"
        roundTrip given expected

bytesNTests :: forall r . Spec r Unit
bytesNTests =
    describe "byteN tests" do

      it "can encode Bytes1" do
         let mgiven = (fromByteString <<<  unsafePartial fromJust $ flip BS.fromString BS.Hex $ "cf") :: Maybe (BytesN D1)
             given = unsafePartial $ fromJust mgiven
             expected = unsafePartial fromJust <<< mkHexString $ "cf00000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode Bytes3" do
         let mgiven =  (fromByteString $ unsafePartial $ fromJust $ flip BS.fromString BS.Hex $ "cf0011") :: Maybe (BytesN D3)
             given = unsafePartial $ fromJust mgiven
             expected = unsafePartial fromJust <<< mkHexString $ "cf00110000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected


      it "can encode Bytes12" do
         let mgiven =  (fromByteString $ unsafePartial $ fromJust $ flip BS.fromString BS.Hex $ "6761766f66796f726b000000") :: Maybe (BytesN (D1 :& D2))
             given = unsafePartial $ fromJust mgiven
             expected =  unsafePartial fromJust <<< mkHexString $ "6761766f66796f726b0000000000000000000000000000000000000000000000"
         roundTrip given expected


intTests :: forall r . Spec r Unit
intTests =
    describe "int/uint tests" do

      it "can encode int" do
         let given = 21
         let expected = unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000015"
         roundTrip given expected

      it "can encode negative numbers" do
         let given = negate 1
         let expected = unsafePartial fromJust <<< mkHexString $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip given expected


      it "can encode some big number" do
         let given = 987654321
         let expected = unsafePartial fromJust <<< mkHexString $ "000000000000000000000000000000000000000000000000000000003ade68b1"
         roundTrip given expected

addressTests :: forall r . Spec r Unit
addressTests =
    describe "addresses tests" do

      it "can encode address" do
         let given = unsafePartial fromJust $ mkAddress =<< mkHexString "407d73d8a49eeb85d32cf465507dd71d507100c1"
         let expected = unsafePartial fromJust <<< mkHexString $ "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c1"
         roundTrip given expected

uintNTests :: forall r . Spec r Unit
uintNTests =
    describe "uint tests" do

      it "can encode uint8" do
         let mgiven =  (uIntNFromBigNumber $ (embed $ 2) `pow` 8 - one) :: Maybe (UIntN D8)
             given = unsafePartial $ fromJust mgiven
             expected = unsafePartial fromJust <<< mkHexString $ "00000000000000000000000000000000000000000000000000000000000000ff"
         roundTrip given expected

      it "can encode larger uint256" do
         let mgiven =  (uIntNFromBigNumber $ ((embed $ 2) `pow` 256) - one) :: Maybe (UIntN (D2 :& (D5 :& D6)))
             given = unsafePartial $ fromJust mgiven
             expected = unsafePartial fromJust <<< mkHexString $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip given expected

      it "can fail to encode larger uin248" do
         let mgiven =  (uIntNFromBigNumber $ (embed $ 2) `pow` 256 - one) :: Maybe (UIntN (D2 :& (D4 :& D8)))
         mgiven `shouldEqual` Nothing

intNTests :: forall r . Spec r Unit
intNTests =
    describe "uint tests" do

      it "can encode int16" do
         let mgiven =  (intNFromBigNumber $ (embed $ negate 1)) :: Maybe (IntN (D1 :& D6))
             given = unsafePartial $ fromJust mgiven
             expected = unsafePartial fromJust <<< mkHexString $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip given expected

      it "can encode larger uint256" do
         let mgiven =  (intNFromBigNumber $ ((embed $ 2) `pow` 255) - one) :: Maybe (IntN (D2 :& D5 :& D6))
             given = unsafePartial $ fromJust mgiven
             expected = unsafePartial fromJust <<< mkHexString $ "7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
         roundTrip given expected

      it "can fail to encode larger int248" do
         let mgiven =  (uIntNFromBigNumber $ (embed $ 2) `pow` 255 - one) :: Maybe (UIntN (D2 :& D4 :& D8))
         mgiven `shouldEqual` Nothing

      it "can fail to encode larger negative int248" do
         let mgiven =  (uIntNFromBigNumber $ negate $ (embed $ 2) `pow` 255 + one) :: Maybe (UIntN (D2 :& D4 :& D8))
         mgiven `shouldEqual` Nothing


falseOrObjectTests :: forall r. Spec r Unit
falseOrObjectTests =
  describe "FalseOrObject tests" do
    let opts = defaultOptions { unwrapSingleConstructors = true }

    it "can decode FalseOrObject instances that are false" do
      let decodedFalse = (runExcept $ decodeJSON "false") :: (Either (NonEmptyList ForeignError) (FalseOrObject SyncStatus))
      decodedFalse `shouldEqual` (Right $ FalseOrObject Nothing)

    it "can decode FalseOrObject instances that are objects" do
      let decodedObj = runExcept $ decodeJSON "{ \"startingBlock\": 0, \"currentBlock\": 1, \"highestBlock\": 2 }"
      decodedObj `shouldEqual` (Right $ FalseOrObject $ Just $ SyncStatus {startingBlock: embed 0, currentBlock: embed 1, highestBlock: embed 2})
