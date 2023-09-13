module Web3Spec.Encoding.ContainersSpec (spec) where

import Prelude

import Control.Monad.Gen (chooseInt, frequency, suchThat)
import Data.Array (foldMap)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Enum (toEnumWithDefaults)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.String (CodePoint, fromCodePointArray)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.BigNumber (BigNumber(..))
import Network.Ethereum.Core.HexString (HexString, toByteString)
import Network.Ethereum.Web3.Solidity (Address, BytesN, IntN, Tuple1(..), Tuple2(..), Tuple4(..), Tuple9(..), UIntN, Vector, fromByteString, intNFromBigNumber, nilVector, uIntNFromBigNumber, (:<))
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncode, class ABIDecode, toDataBuilder, fromData)
import Network.Ethereum.Web3.Solidity.Generic (genericFromData, genericABIEncode, class GenericABIDecode, class GenericABIEncode)
import Network.Ethereum.Web3.Solidity.Sizes (s1, s16, s2, s224, s256, s4)
import Network.Ethereum.Web3.Solidity.Vector (Vector, toVector)
import Network.Ethereum.Web3.Types (Address, HexString, embed, mkAddress, mkHexString, unHex)
import Parsing (ParseError)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, quickCheck', (<?>), (===))
import Test.QuickCheck.Gen (arrayOf, vectorOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "encoding-spec for containers" do
    staticArraysTests
    dynamicArraysTests
    tuplesTest
    typePropertyTests
    arrayTypePropertyTests
    vecTypePropertyTests
    nestedTypePropertyTests

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
  let
    a = toDataBuilder x
  in
    (fromData a)

--tuplePropertyTests :: Spec Unit
--tuplePropertyTests =
--  describe "Tuple property tests" do
--    it "can encode and decode a Tuple1" $ liftEffect $ do
--      quickCheck \(x :: Tuple1 Int) ->

typePropertyTests :: Spec Unit
typePropertyTests =
  describe "Type property tests" do
    it "can encode and decode a string" $ liftEffect $ do
      quickCheck \(BMPString x) -> (encodeDecode x) === Right x
    it "can encode and decode a bytestring" $ liftEffect $ do
      quickCheck \(_x :: HexString) ->
        let
          x = toByteString _x
        in
          (encodeDecode x) === Right x
    it "can encode and decode a bool" $ liftEffect $ do
      quickCheck \(x :: Boolean) -> encodeDecode x === Right x
    it "can encode and decode an address" $ liftEffect $ do
      quickCheck \(x :: Address) -> encodeDecode x === Right x
    it "can encode and decode an int8" $ liftEffect $ do
      quickCheck \(x :: IntN 8) -> encodeDecode x === Right x
    it "can encode and decode an int16" $ liftEffect $ do
      quickCheck \(x :: IntN 16) -> encodeDecode x === Right x
    it "can encode and decode an int32" $ liftEffect $ do
      quickCheck \(x :: IntN 32) -> encodeDecode x === Right x
    it "can encode and decode an int64" $ liftEffect $ do
      quickCheck \(x :: IntN 64) -> encodeDecode x === Right x
    it "can encode and decode an int128" $ liftEffect $ do
      quickCheck \(x :: IntN 128) -> encodeDecode x === Right x
    it "can encode and decode an int256" $ liftEffect $ do
      quickCheck \(x :: IntN 256) -> encodeDecode x === Right x
    it "can encode and decode an uint8" $ liftEffect $ do
      quickCheck \(x :: UIntN 8) -> encodeDecode x === Right x
    it "can encode and decode an uint16" $ liftEffect $ do
      quickCheck \(x :: UIntN 16) -> encodeDecode x === Right x
    it "can encode and decode an uint32" $ liftEffect $ do
      quickCheck \(x :: UIntN 32) -> encodeDecode x === Right x
    it "can encode and decode an uint64" $ liftEffect $ do
      quickCheck \(x :: UIntN 64) -> encodeDecode x === Right x
    it "can encode and decode an uint128" $ liftEffect $ do
      quickCheck \(x :: UIntN 128) -> encodeDecode x === Right x
    it "can encode and decode an uint256" $ liftEffect $ do
      quickCheck \(x :: UIntN 256) -> encodeDecode x === Right x
    it "can encode and decode bytes1" $ liftEffect $ do
      quickCheck \(x :: BytesN 1) -> encodeDecode x === Right x
    it "can encode and decode bytes2" $ liftEffect $ do
      quickCheck \(x :: BytesN 2) -> encodeDecode x === Right x
    it "can encode and decode bytes3" $ liftEffect $ do
      quickCheck \(x :: BytesN 3) -> encodeDecode x === Right x
    it "can encode and decode bytes4" $ liftEffect $ do
      quickCheck \(x :: BytesN 4) -> encodeDecode x === Right x
    it "can encode and decode bytes5" $ liftEffect $ do
      quickCheck \(x :: BytesN 5) -> encodeDecode x === Right x
    it "can encode and decode bytes6" $ liftEffect $ do
      quickCheck \(x :: BytesN 6) -> encodeDecode x === Right x
    it "can encode and decode bytes7" $ liftEffect $ do
      quickCheck \(x :: BytesN 7) -> encodeDecode x === Right x
    it "can encode and decode bytes8" $ liftEffect $ do
      quickCheck \(x :: BytesN 8) -> encodeDecode x === Right x
    it "can encode and decode bytes9" $ liftEffect $ do
      quickCheck \(x :: BytesN 9) -> encodeDecode x === Right x
    it "can encode and decode bytes10" $ liftEffect $ do
      quickCheck \(x :: BytesN 10) -> encodeDecode x === Right x
    it "can encode and decode bytes11" $ liftEffect $ do
      quickCheck \(x :: BytesN 11) -> encodeDecode x === Right x
    it "can encode and decode bytes12" $ liftEffect $ do
      quickCheck \(x :: BytesN 12) -> encodeDecode x === Right x
    it "can encode and decode bytes13" $ liftEffect $ do
      quickCheck \(x :: BytesN 13) -> encodeDecode x === Right x
    it "can encode and decode bytes14" $ liftEffect $ do
      quickCheck \(x :: BytesN 14) -> encodeDecode x === Right x
    it "can encode and decode bytes15" $ liftEffect $ do
      quickCheck \(x :: BytesN 15) -> encodeDecode x === Right x
    it "can encode and decode bytes16" $ liftEffect $ do
      quickCheck \(x :: BytesN 16) -> encodeDecode x === Right x
    it "can encode and decode bytes17" $ liftEffect $ do
      quickCheck \(x :: BytesN 17) -> encodeDecode x === Right x
    it "can encode and decode bytes18" $ liftEffect $ do
      quickCheck \(x :: BytesN 18) -> encodeDecode x === Right x
    it "can encode and decode bytes19" $ liftEffect $ do
      quickCheck \(x :: BytesN 19) -> encodeDecode x === Right x
    it "can encode and decode bytes20" $ liftEffect $ do
      quickCheck \(x :: BytesN 20) -> encodeDecode x === Right x
    it "can encode and decode bytes21" $ liftEffect $ do
      quickCheck \(x :: BytesN 21) -> encodeDecode x === Right x
    it "can encode and decode bytes22" $ liftEffect $ do
      quickCheck \(x :: BytesN 22) -> encodeDecode x === Right x
    it "can encode and decode bytes23" $ liftEffect $ do
      quickCheck \(x :: BytesN 23) -> encodeDecode x === Right x
    it "can encode and decode bytes24" $ liftEffect $ do
      quickCheck \(x :: BytesN 24) -> encodeDecode x === Right x
    it "can encode and decode bytes25" $ liftEffect $ do
      quickCheck \(x :: BytesN 25) -> encodeDecode x === Right x
    it "can encode and decode bytes26" $ liftEffect $ do
      quickCheck \(x :: BytesN 26) -> encodeDecode x === Right x
    it "can encode and decode bytes27" $ liftEffect $ do
      quickCheck \(x :: BytesN 27) -> encodeDecode x === Right x
    it "can encode and decode bytes28" $ liftEffect $ do
      quickCheck \(x :: BytesN 28) -> encodeDecode x === Right x
    it "can encode and decode bytes29" $ liftEffect $ do
      quickCheck \(x :: BytesN 29) -> encodeDecode x === Right x
    it "can encode and decode bytes30" $ liftEffect $ do
      quickCheck \(x :: BytesN 30) -> encodeDecode x === Right x
    it "can encode and decode bytes31" $ liftEffect $ do
      quickCheck \(x :: BytesN 31) -> encodeDecode x === Right x
    it "can encode and decode bytes32" $ liftEffect $ do
      quickCheck \(x :: BytesN 32) -> encodeDecode x === Right x

arrayTypePropertyTests :: Spec Unit
arrayTypePropertyTests = do
  describe "Array type property tests: int" do
    it "Can do arrays of int8" $ liftEffect do
      quickCheck $ \(x :: Array (IntN 8)) -> encodeDecode x === Right x
    it "Can do arrays of int32" $ liftEffect do
      quickCheck $ \(x :: Array (IntN 32)) -> encodeDecode x === Right x
    it "Can do arrays of int256" $ liftEffect do
      quickCheck $ \(x :: Array (IntN 256)) -> encodeDecode x === Right x

  describe "Array type property tests: uint" do
    it "Can do arrays of uint16" $ liftEffect do
      quickCheck $ \(x :: Array (UIntN 16)) -> encodeDecode x === Right x
    it "Can do arrays of uint24" $ liftEffect do
      quickCheck $ \(x :: Array (UIntN 24)) -> encodeDecode x === Right x
    it "Can do arrays of uint64" $ liftEffect do
      quickCheck $ \(x :: Array (UIntN 64)) -> encodeDecode x === Right x

  describe "Array type property tests: address" do
    it "Can do arrays of address" $ liftEffect do
      quickCheck $ \(x :: Address) -> encodeDecode x === Right x

  describe "Array type property tests: string" do
    it "Can do arrays of address" $ liftEffect do
      quickCheck $ \(_x :: Array BMPString) ->
        let
          x = map (\(BMPString s) -> s) _x
        in
          encodeDecode x === Right x

vecTypePropertyTests :: Spec Unit
vecTypePropertyTests = do
  describe "Vec type property tests: int[N]" do
    it "Can do vec of int40[1]" $ liftEffect do
      quickCheck $ \(x :: Vector 1 (IntN 40)) -> encodeDecode x === Right x
    it "Can do vec of int112[5]" $ liftEffect do
      quickCheck $ \(x :: Vector 5 (IntN 112)) -> encodeDecode x === Right x
    it "Can do vec of int168[10]" $ liftEffect do
      quickCheck $ \(x :: Vector 10 (IntN 168)) -> encodeDecode x === Right x

  describe "Array type property tests: uint[N]" do
    it "Can do vec of uint16" $ liftEffect do
      quickCheck $ \(x :: Vector 2 (UIntN 144)) -> encodeDecode x === Right x
    it "Can do vec of uint24" $ liftEffect do
      quickCheck $ \(x :: Vector 4 (UIntN 176)) -> encodeDecode x === Right x
    it "Can do vec of uint24" $ liftEffect do
      quickCheck $ \(x :: Vector 7 (UIntN 192)) -> encodeDecode x === Right x

  describe "Array type property tests: address[N]" do
    it "Can do vec of address" $ liftEffect do
      quickCheck $ \(x :: Vector 3 Address) -> encodeDecode x === Right x
    it "Can do vec of address" $ liftEffect do
      quickCheck $ \(x :: Vector 6 Address) -> encodeDecode x === Right x

  describe "Array type property tests: string[N]" do
    it "Can do vec of address" $ liftEffect do
      quickCheck $ \(_x :: Vector 3 BMPString) ->
        let
          x = map (\(BMPString s) -> s) _x
        in
          encodeDecode x === Right x
    it "Can do vec of string" $ liftEffect do
      quickCheck $ \(_x :: Vector 4 BMPString) ->
        let
          x = map (\(BMPString s) -> s) _x
        in
          encodeDecode x === Right x

nestedTypePropertyTests :: Spec Unit
nestedTypePropertyTests = do
  describe "Nested type property tests for fixed size things" do
    it "Can do address[4][]" $ liftEffect do
      quickCheck \(x :: Array (Vector 4 Address)) -> encodeDecode x === Right x
    it "Can do int64[][5]" $ liftEffect do
      quickCheck \(x :: Vector 4 (IntN 64)) -> encodeDecode x === Right x
    it "Can do uint[3][4]" $ liftEffect do
      quickCheck \(x :: Vector 4 (Vector 3 (UIntN 256))) -> encodeDecode x === Right x
    it "Can do bytes12[][]" $ liftEffect do
      quickCheck \(x :: Array (Array (BytesN 32))) -> encodeDecode x === Right x

  describe "Nested type property tests for variable size things" do
    it "Can do string[4][]" $ liftEffect do
      quickCheck \(x :: Array (Vector 4 String)) -> encodeDecode x === Right x
    it "Can do bytes[][5]" $ liftEffect do
      quickCheck \(_x :: Vector 4 (Array HexString)) ->
        let
          x = map (map toByteString) _x
        in
          encodeDecode x === Right x
    it "Can do string[2][5]" $ liftEffect do
      quickCheck \(x :: Vector 5 (Vector 2 String)) -> encodeDecode x === Right x
    it "Can do bytes[][]" $ liftEffect do
      quickCheck \(_x :: Array (Array HexString)) ->
        let
          x = map (map toByteString) _x
        in
          encodeDecode x === Right x

--------------------------------------------------------------------------------
newtype BMPString = BMPString String

data UnicodeChar = Normal CodePoint | Surrogates CodePoint CodePoint

instance Arbitrary BMPString where
  arbitrary = BMPString <$> do
    n <- chooseInt 0 10
    ucs <- vectorOf n arbitrary
    pure $ fromCodePointArray $ foldMap f ucs
    where
    f uc = case uc of
      Normal a -> [ a ]
      Surrogates a b -> [ a, b ]

instance Arbitrary UnicodeChar where
  arbitrary = frequency $ NonEmpty (Tuple (1.0 - p) normalGen) [ Tuple p surrogatesGen ]

    where
    hiLB = 0xD800
    hiUB = 0xDBFF
    loLB = 0xDC00
    loUB = 0xDFFF
    maxCP = 65535
    toCP = toEnumWithDefaults bottom top
    -- must have a high surrogate followed by a low surrogate
    surrogatesGen = Surrogates <$> (toCP <$> chooseInt hiLB hiUB) <*> (toCP <$> chooseInt loLB loUB)
    normalGen = Normal <<< toCP <$> do
      chooseInt 0 maxCP `suchThat` \n ->
        (n < hiLB || n > hiUB) && (n < loLB || n > loUB)
    -- probability that you pick a surrogate from all possible codepoints
    p = toNumber ((hiUB - hiLB + 1) + (loUB - loLB + 1)) / toNumber (maxCP + 1)