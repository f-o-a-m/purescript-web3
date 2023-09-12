module Web3Spec.Encoding.ContainersSpec (spec) where

import Prelude

import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3.Solidity (BytesN, IntN, Tuple1(..), Tuple2(..), Tuple4(..), Tuple9(..), UIntN, fromByteString, intNFromBigNumber, nilVector, toDataBuilder, uIntNFromBigNumber, (:<))
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncode, class ABIDecode, toDataBuilder, fromData)
import Network.Ethereum.Web3.Solidity.Generic (genericFromData, genericABIEncode, class GenericABIDecode, class GenericABIEncode)
import Network.Ethereum.Web3.Solidity.Sizes (s1, s16, s2, s224, s256, s4)
import Network.Ethereum.Web3.Solidity.Vector (Vector, toVector)
import Network.Ethereum.Web3.Types (Address, HexString, embed, mkAddress, mkHexString, unHex)
import Parsing (ParseError)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (quickCheck, (<?>), (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "encoding-spec for containers" do
    --staticArraysTests
    --dynamicArraysTests
    --tuplesTest
    typePropertyTests

roundTrip :: forall a. Show a => Eq a => ABIEncode a => ABIDecode a => a -> HexString -> Aff Unit
roundTrip decoded encoded = do
  encoded `shouldEqual` toDataBuilder decoded
  fromData encoded `shouldEqual` Right decoded

roundTripGeneric
  :: forall a rep
   . Show a
  => Eq a
  => Generic a rep
  => GenericABIEncode rep
  => GenericABIDecode rep
  => a
  -> HexString
  -> Aff Unit
roundTripGeneric decoded encoded = do
  encoded `shouldEqual` genericABIEncode decoded
  genericFromData encoded `shouldEqual` Right decoded

staticArraysTests :: Spec Unit
staticArraysTests =
  describe "statically sized array tests" do
    it "can encode statically sized vectors of addresses" do
      let
        mgivenElement = toVector s1 $ [ false ]

        givenElement = (unsafePartial fromJust $ mgivenElement)

        given = (unsafePartial fromJust $ toVector s2 [ givenElement, givenElement ])

        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected
    it "can encode statically sized vectors of statically sized vectors of type bool" do
      let
        mgiven =
          toVector s2
            $ map (\a -> unsafePartial fromJust $ mkAddress =<< mkHexString a)
                [ "407d73d8a49eeb85d32cf465507dd71d507100c1"
                , "407d73d8a49eeb85d32cf465507dd71d507100c3"
                ]

        given = (unsafePartial $ fromJust $ mgiven) :: Vector 2 Address

        expected =
          unsafePartial (fromJust <<< mkHexString) $ "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c1"
            <> "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c3"
      roundTrip given expected
    it "can encode statically sized vectors of statically sized bytes" do
      let
        elem1 = unsafePartial fromJust (fromByteString s1 =<< flip BS.fromString BS.Hex "cf")

        elem2 = unsafePartial fromJust (fromByteString s1 =<< flip BS.fromString BS.Hex "68")

        elem3 = unsafePartial fromJust (fromByteString s1 =<< flip BS.fromString BS.Hex "4d")

        elem4 = unsafePartial fromJust (fromByteString s1 =<< flip BS.fromString BS.Hex "fb")

        given = unsafePartial fromJust (toVector s4 $ [ elem1, elem2, elem3, elem4 ]) :: Vector 4 (BytesN 1)

        expected =
          unsafePartial (fromJust <<< mkHexString)
            $ "cf00000000000000000000000000000000000000000000000000000000000000"
                <> "6800000000000000000000000000000000000000000000000000000000000000"
                <> "4d00000000000000000000000000000000000000000000000000000000000000"
                <> "fb00000000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected

dynamicArraysTests :: Spec Unit
dynamicArraysTests =
  describe "dynamically sized array tests" do
    it "can encode dynamically sized lists of bools" do
      let
        given = [ true, true, false ]

        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000003"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTrip given expected

tuplesTest :: Spec Unit
tuplesTest =
  describe "tuples test" do
    it "can encode 2-tuples with both static args" do
      let
        given = Tuple2 true false

        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTripGeneric given expected
    it "can encode 1-tuples with dynamic arg" do
      let
        given = Tuple1 [ true, false ]

        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000020"
            <> "0000000000000000000000000000000000000000000000000000000000000002"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
      roundTripGeneric given expected
    it "can encode 4-tuples with a mix of args -- (UInt, String, Boolean, Array Int)" do
      let
        given = Tuple4 1 "dave" true [ 1, 2, 3 ]

        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000080"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "00000000000000000000000000000000000000000000000000000000000000c0"
            <> "0000000000000000000000000000000000000000000000000000000000000004"
            <> "6461766500000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000003"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000002"
            <> "0000000000000000000000000000000000000000000000000000000000000003"
      roundTripGeneric given expected
    it "can do something really complicated" do
      let
        uint = unsafePartial $ fromJust $ uIntNFromBigNumber s256 $ embed 1

        int = unsafePartial $ fromJust $ intNFromBigNumber s256 $ embed $ negate 1

        bool = true

        int224 = unsafePartial $ fromJust $ intNFromBigNumber s224 $ embed 221

        bools = true :< false :< nilVector

        ints =
          [ unsafePartial $ fromJust $ intNFromBigNumber s256 $ embed 1
          , unsafePartial $ fromJust $ intNFromBigNumber s256 $ embed $ negate 1
          , unsafePartial $ fromJust $ intNFromBigNumber s256 $ embed 3
          ]

        string = "hello"

        bytes16 = unsafePartial fromJust $ fromByteString s16 =<< flip BS.fromString BS.Hex "12345678123456781234567812345678"

        elem = unsafePartial fromJust $ fromByteString s2 =<< flip BS.fromString BS.Hex "1234"

        vector4 = elem :< elem :< elem :< elem :< nilVector

        bytes2s = [ vector4, vector4 ]

        given =
          Tuple9 uint int bool int224 bools ints string bytes16 bytes2s
            :: Tuple9 (UIntN 256)
                 (IntN 256)
                 Boolean
                 (IntN 224)
                 (Vector 2 Boolean)
                 (Array (IntN 256))
                 String
                 (BytesN 16)
                 (Array (Vector 4 (BytesN 2)))

        expected =
          unsafePartial fromJust <<< mkHexString $ "0000000000000000000000000000000000000000000000000000000000000001"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "00000000000000000000000000000000000000000000000000000000000000dd"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "0000000000000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000140"
            <> "00000000000000000000000000000000000000000000000000000000000001c0"
            <> "1234567812345678123456781234567800000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000200"
            <> "0000000000000000000000000000000000000000000000000000000000000003"
            <> "0000000000000000000000000000000000000000000000000000000000000001"
            <> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            <> "0000000000000000000000000000000000000000000000000000000000000003"
            <> "0000000000000000000000000000000000000000000000000000000000000005"
            <> "68656c6c6f000000000000000000000000000000000000000000000000000000"
            <> "0000000000000000000000000000000000000000000000000000000000000002"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
            <> "1234000000000000000000000000000000000000000000000000000000000000"
      roundTripGeneric given expected

encodeDecodeGeneric
  :: forall a rep
   . Show a
  => Eq a
  => Generic a rep
  => GenericABIEncode rep
  => GenericABIDecode rep
  => a
  -> Either ParseError a
encodeDecodeGeneric x = genericFromData $ genericABIEncode x

encodeDecode
  :: forall a
   . Show a
  => Eq a
  => ABIEncode a
  => ABIDecode a
  => a
  -> Either ParseError a
encodeDecode x =
  fromData $ toDataBuilder x

--tuplePropertyTests :: Spec Unit
--tuplePropertyTests =
--  describe "Tuple property tests" do
--    it "can encode and decode a Tuple1" $ liftEffect $ do
--      quickCheck \(x :: Tuple1 Int) ->

typePropertyTests :: Spec Unit
typePropertyTests =
  describe "Type property tests" do
    it "can encode and decode an Int" $ liftEffect $ do
      quickCheck \(x :: Int) ->
        encodeDecode x
          ==
            Right x
          <?> ("x: " <> show x <> ", encoded: " <> unHex (toDataBuilder x) <> ", decoded: " <> show (encodeDecode x))