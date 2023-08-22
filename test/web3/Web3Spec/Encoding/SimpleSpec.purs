module Web3Spec.Encoding.SimpleSpec (spec) where

import Prelude
import Effect.Aff (Aff, error, throwError)
import Control.Monad.Except (runExcept)
import Data.Array (replicate)
import Data.ByteString as BS
import Data.Either (Either(Right), either)
import Data.Foldable (intercalate)
import Foreign (ForeignError)
import Foreign.Generic (decodeJSON)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.String (toLower)
import Data.Traversable (sequence)
import Network.Ethereum.Core.BigNumber (pow)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncode, class ABIDecode, toDataBuilder, fromData)
import Network.Ethereum.Web3.Solidity.Bytes (fromByteString)
import Network.Ethereum.Web3.Solidity.Int (intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s1, s12, s16, s248, s256, s3, s8)
import Network.Ethereum.Web3.Solidity.UInt (uIntNFromBigNumber)
import Network.Ethereum.Web3.Types (Block, FalseOrObject(..), HexString, BigNumber, SyncStatus(..), embed, mkAddress, mkHexString, unHex)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

spec :: Spec Unit
spec =
  describe "encoding-spec" do
    stringTests
    bytesDTests
    bytesNTests
    intTests
    uintNTests
    intNTests
    addressTests
    falseOrObjectTests
    blockTests

roundTrip :: forall a. Show a => Eq a => ABIEncode a => ABIDecode a => a -> HexString -> Aff Unit
roundTrip decoded encoded = do
  encoded `shouldEqual` toDataBuilder decoded
  fromData encoded `shouldEqual` Right decoded

stringTests :: Spec Unit
stringTests =
  describe "string tests" do
    it "can encode simple strings" do
      let
        given = "gavofyork"
      let
        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000009"
            <> "6761766f66796f726b0000000000000000000000000000000000000000000000"
      roundTrip given expected
    it "can encode complicated strings" do
      let
        given = "welcome to ethereum. welcome to ethereum. welcome to ethereum."
      let
        expected =
          unsafePartial fromJust <<< mkHexString $ "000000000000000000000000000000000000000000000000000000000000003e"
            <> "77656c636f6d6520746f20657468657265756d2e2077656c636f6d6520746f20"
            <> "657468657265756d2e2077656c636f6d6520746f20657468657265756d2e0000"
      roundTrip given expected
    it "can encode unicode strings" do
      let
        given = "Ã¤Ã¤"
      let
        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000008"
            <> "c383c2a4c383c2a4000000000000000000000000000000000000000000000000"
      roundTrip given expected
    it "can handle VERY long HexStrings" do
      let
        given = intercalate "" $ replicate 128 "0000000000000000000000000000000000000000000000000000000000000000"
      let
        expected = unsafePartial fromJust <<< mkHexString $ given
      given `shouldEqual` unHex expected
    it "can handle mixed case HexStrings" do
      let
        given = "fF"
      let
        expected = unsafePartial fromJust <<< mkHexString $ given
      -- note; for easy equality we should canonicalize HexStrings as lowercase
      toLower given `shouldEqual` unHex expected
    it "fails on odd length HexStrings" do
      let
        givens = [ "f", "0", "000", "0f0", "fffff", "0000000000000000000000000000000000000000f" ]
      _ <- sequence $ map (\g -> mkHexString g `shouldEqual` Nothing) givens
      pure unit
    it "should hold equality across cases" do
      mkHexString "ff" `shouldEqual` mkHexString "Ff"
      mkHexString "0000aa" `shouldEqual` mkHexString "0000AA"
      mkHexString "0000aa" `shouldEqual` mkHexString "0000aa"
      mkHexString "abcdef" `shouldEqual` mkHexString "AbCdEf"
      mkHexString "" `shouldNotEqual` mkHexString "ff"
      mkHexString "ff" `shouldNotEqual` mkHexString "aa"

bytesDTests :: Spec Unit
bytesDTests =
  describe "bytesD tests" do
    it "can encode short bytesD" do
      let
        given = unsafePartial $ fromJust $ flip BS.fromString BS.Hex $ "c3a40000c3a4"
      let
        expected =
          unsafePartial fromJust <<< mkHexString
            $ "0000000000000000000000000000000000000000000000000000000000000006"
            <> "c3a40000c3a40000000000000000000000000000000000000000000000000000"
      roundTrip given expected
    it "can encode long bytesD" do
      let
        given =
          unsafePartial $ fromJust $ flip BS.fromString BS.Hex
            $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1"
      let
        expected =
          unsafePartial fromJust <<< mkHexString
            $ "000000000000000000000000000000000000000000000000000000000000009f"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff100"
      roundTrip given expected
    it "can encode dave" do
      let
        given = "dave"
      let
        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000004"
            <> "6461766500000000000000000000000000000000000000000000000000000000"
      roundTrip given expected

bytesNTests :: Spec Unit
bytesNTests =
  describe "byteN tests" do
    it "can encode Bytes1" do
      let
        mgiven = fromByteString s1 <<< unsafePartial fromJust $ flip BS.fromString BS.Hex $ "cf"

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "cf00000000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected
    it "can encode Bytes3" do
      let
        mgiven = fromByteString s3 $ unsafePartial $ fromJust $ flip BS.fromString BS.Hex $ "cf0011"

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "cf00110000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected
    it "can encode Bytes12" do
      let
        mgiven = fromByteString s12 $ unsafePartial $ fromJust $ flip BS.fromString BS.Hex $ "6761766f66796f726b000000"

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "6761766f66796f726b0000000000000000000000000000000000000000000000"
      roundTrip given expected

intTests :: Spec Unit
intTests =
  describe "int/uint tests" do
    it "can encode int" do
      let
        given = 21
      let
        expected = unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000015"
      roundTrip given expected
    it "can encode negative numbers" do
      let
        given = negate 1
      let
        expected = unsafePartial fromJust <<< mkHexString $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      roundTrip given expected
    it "can encode some big number" do
      let
        given = 987654321
      let
        expected = unsafePartial fromJust <<< mkHexString $ "000000000000000000000000000000000000000000000000000000003ade68b1"
      roundTrip given expected

addressTests :: Spec Unit
addressTests =
  describe "addresses tests" do
    it "can encode address" do
      let
        given = unsafePartial fromJust $ mkAddress =<< mkHexString "407d73d8a49eeb85d32cf465507dd71d507100c1"
      let
        expected = unsafePartial fromJust <<< mkHexString $ "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c1"
      roundTrip given expected

uintNTests :: Spec Unit
uintNTests =
  describe "uint tests" do
    it "can encode uint8" do
      let
        mgiven = uIntNFromBigNumber s8 $ (embed 2) `pow` 8 - one

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "00000000000000000000000000000000000000000000000000000000000000ff"
      roundTrip given expected
    it "can encode larger uint256" do
      let
        mgiven = uIntNFromBigNumber s256 $ ((embed $ 2) `pow` 256) - one

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      roundTrip given expected
    it "can fail to encode larger uin248" do
      let
        mgiven = (uIntNFromBigNumber s248 $ (embed $ 2) `pow` 256 - one)
      mgiven `shouldEqual` Nothing

intNTests :: Spec Unit
intNTests =
  describe "uint tests" do
    it "can encode int16" do
      let
        mgiven = intNFromBigNumber s16 $ embed $ negate 1

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      roundTrip given expected
    it "can encode larger uint256" do
      let
        mgiven = intNFromBigNumber s256 $ ((embed $ 2) `pow` 255) - one

        given = unsafePartial $ fromJust mgiven

        expected = unsafePartial fromJust <<< mkHexString $ "7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      roundTrip given expected
    it "can fail to encode larger int248" do
      let
        mgiven = uIntNFromBigNumber s248 $ (embed $ 2) `pow` 255 - one
      mgiven `shouldEqual` Nothing
    it "can fail to encode larger negative int248" do
      let
        mgiven = uIntNFromBigNumber s248 $ negate $ (embed $ 2) `pow` 255 + one
      mgiven `shouldEqual` Nothing

falseOrObjectTests :: Spec Unit
falseOrObjectTests =
  describe "FalseOrObject tests" do
    it "can decode FalseOrObject instances that are false" do
      let
        decodedFalse = (runExcept $ decodeJSON "false") :: (Either (NonEmptyList ForeignError) (FalseOrObject SyncStatus))
      decodedFalse `shouldEqual` (Right $ FalseOrObject Nothing)
    it "can decode FalseOrObject instances that are objects" do
      let
        decodedObj = runExcept $ decodeJSON "{ \"startingBlock\": \"0x0\", \"currentBlock\": \"0x1\", \"highestBlock\": \"0x2\" }"
      decodedObj `shouldEqual` (Right $ FalseOrObject $ Just $ SyncStatus { startingBlock: embed 0, currentBlock: embed 1, highestBlock: embed 2 })

blockTests :: Spec Unit
blockTests =
  describe "Block decoding tests" do
    it "can decode normal blocks" do
      let
        (decodedBlockE :: Either (NonEmptyList ForeignError) Block) = runExcept $ decodeJSON blockPlaintext
      dBlock <- unwrap <$> either (throwError <<< error <<< show) pure decodedBlockE
      dBlock.nonce `shouldEqual` (Just $ upToHex "0x0000000000000000")
      dBlock.hash `shouldEqual` (Just $ upToHex "0x093ff26b85b5e3ac3e331f3d766a81990be76ec8ac79f62a81e30faa642dc26f")
      dBlock.timestamp `shouldEqual` embed 1507570522
  where
  -- this is block 1 on Eth mainnet
  blockPlaintext = "{\"difficulty\":\"0x1\",\"extraData\":\"0x0000000000000000000000000000000000000000000000000000000000000000759e3fae48d5abad53ab446f31ab3ae1531f2e4c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"gasLimit\":\"0x8000000\",\"gasUsed\":\"0x0\",\"hash\":\"0x093ff26b85b5e3ac3e331f3d766a81990be76ec8ac79f62a81e30faa642dc26f\",\"logsBloom\":\"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"mixHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"number\":\"0x0\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"size\":\"0x273\",\"stateRoot\":\"0xd3811ce828cfc6b07dbedfe073e1ef7e50bda2dac61a901e995c0f460a625cdd\",\"timestamp\":\"0x59dbb35a\",\"totalDifficulty\":\"0x1\",\"transactions\":[],\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"uncles\":[]}"

  upToHex = unsafePartial fromJust <<< mkHexString

newtype KovanBlock
  = KovanBlock
  { difficulty :: BigNumber
  , extraData :: HexString
  , gasLimit :: BigNumber
  , gasUsed :: BigNumber
  , hash :: Maybe HexString
  , logsBloom :: Maybe HexString
  , author :: HexString
  , sealFields :: Array HexString
  , number :: Maybe BigNumber
  , parentHash :: HexString
  , receiptsRoot :: HexString
  , sha3Uncles :: HexString
  , size :: BigNumber
  , stateRoot :: HexString
  , timestamp :: BigNumber
  , totalDifficulty :: BigNumber
  , transactions :: Array HexString
  , transactionsRoot :: HexString
  , uncles :: Array HexString
  }
