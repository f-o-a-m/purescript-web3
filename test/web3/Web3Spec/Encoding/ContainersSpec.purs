module Web3Spec.Encoding.ContainersSpec (spec) where

import Prelude

import Control.Monad.Gen (chooseInt, frequency, suchThat)
import Data.Array (filter, foldMap, (..))
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Enum (toEnumWithDefaults)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.NonEmpty (NonEmpty(..))
import Data.Reflectable (reifyType)
import Data.String (CodePoint, fromCodePointArray)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (toByteString)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncode, class ABIDecode, toDataBuilder, fromData)
import Network.Ethereum.Web3.Solidity.Bytes as BytesN
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType)
import Network.Ethereum.Web3.Solidity.Int as IntN
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Network.Ethereum.Web3.Solidity.Vector as Vector
import Network.Ethereum.Web3.Types (Address, HexString)
import Parsing (ParseError)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, quickCheckGen, (===))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "encoding-spec for containers" do
    typePropertyTests
    arrayTypePropertyTests
    vecTypePropertyTests
    nestedTypePropertyTests

typePropertyTests :: Spec Unit
typePropertyTests =
  describe "Type property tests" do
    it "can encode/decode a string" $ liftEffect $ do
      quickCheck \(x :: BMPString) -> (encodeDecode x) === Right x

    it "can encode/decode bytestring" $ liftEffect $ do
      quickCheck \(_x :: HexString) ->
        let
          x = toByteString _x
        in
          (encodeDecode x) === Right x

    it "can encode/decode bool" $ liftEffect $ do
      quickCheck \(x :: Boolean) -> encodeDecode x === Right x

    it "can encode/decode address" $ liftEffect $ do
      quickCheck \(x :: Address) -> encodeDecode x === Right x

    it "can encode/decode intN" $ liftEffect $ do
      for_ intSizes $ \n -> quickCheckGen $ do
        reifyType n \p -> do
          x <- IntN.generator p
          pure $ encodeDecode x === Right x

    it "can encode/decode uintN" $ liftEffect $ do
      for_ intSizes $ \n -> quickCheckGen $ do
        reifyType n \p -> do
          x <- UIntN.generator p
          pure $ encodeDecode x === Right x

    it "can encode/decode bytesN" $ liftEffect $ do
      for_ bytesSizes $ \n -> quickCheckGen $ do
        reifyType n \p -> do
          x <- BytesN.generator p
          pure $ encodeDecode x === Right x

    it "can encode/decode string" $ liftEffect $ do
      quickCheck \(x :: BMPString) -> encodeDecode x === Right x

arrayTypePropertyTests :: Spec Unit
arrayTypePropertyTests = do

  describe "Array type property tests" do

    it "Can encode/decode intN[]" $ liftEffect do
      for_ intSizes $ \n -> quickCheckGen $ do
        reifyType n \p -> do
          x <- arrayOf (IntN.generator p)
          pure $ encodeDecode x === Right x

    it "Can encode/decode uintN[]" $ liftEffect do
      for_ intSizes $ \n -> quickCheckGen $ do
        reifyType n \p -> do
          x <- arrayOf (UIntN.generator p)
          pure $ encodeDecode x === Right x

    it "Can encode/decode bytesN[]" $ liftEffect do
      for_ bytesSizes $ \n -> quickCheckGen $ do
        reifyType n \p -> do
          x <- arrayOf (BytesN.generator p)
          pure $ encodeDecode x === Right x

    it "Can encode/decode address[]" $ liftEffect do
      quickCheck $ \(x :: Address) -> encodeDecode x === Right x

    it "Can encode/decode string[]" $ liftEffect do
      quickCheck $ \(x :: Array BMPString) ->
        encodeDecode x === Right x

vecTypePropertyTests :: Spec Unit
vecTypePropertyTests = do

  describe "Vector type property tests" do

    it "Can encode/decode intN[k]" $ liftEffect do
      for_ intSizes $ \n ->
        quickCheckGen $ do
          k <- chooseInt 1 10
          reifyType k \pk ->
            reifyType n \pn -> do
              x <- Vector.generator pk (IntN.generator pn)
              pure $ encodeDecode x === Right x

    it "Can encode/decode uintN[k]" $ liftEffect do
      for_ intSizes $ \n ->
        quickCheckGen $ do
          k <- chooseInt 1 10
          reifyType k \pk ->
            reifyType n \pn -> do
              x <- Vector.generator pk (UIntN.generator pn)
              pure $ encodeDecode x === Right x

    it "Can encode/decode bytesN[k]" $ liftEffect do
      for_ bytesSizes $ \n ->
        quickCheckGen $ do
          k <- chooseInt 1 10
          reifyType k \pk ->
            reifyType n \pn -> do
              x <- Vector.generator pk (BytesN.generator pn)
              pure $ encodeDecode x === Right x

    it "Can encode/decode address[k]" $ liftEffect do
      quickCheckGen $ do
        k <- chooseInt 1 10
        reifyType k \pk -> do
          x <- Vector.generator pk (arbitrary :: Gen Address)
          pure $ encodeDecode x === Right x

    it "Can encode/decode string[k]" $ liftEffect do
      quickCheckGen $ do
        k <- chooseInt 1 10
        reifyType k \pk -> do
          x <- Vector.generator pk (arbitrary :: Gen BMPString)
          pure $ encodeDecode x === Right x

nestedTypePropertyTests :: Spec Unit
nestedTypePropertyTests = do
  describe "Nested type property tests for vector, vector" do

    it "Can encode/decode bytesN[k1][k2]" $ liftEffect do
      for_ bytesSizes $ \n -> do
        quickCheckGen $ do
          k1 <- chooseInt 1 10
          k2 <- chooseInt 1 10
          reifyType k1 \pk1 ->
            reifyType k2 \pk2 ->
              reifyType n \pn -> do
                x <- Vector.generator pk2 (Vector.generator pk1 (BytesN.generator pn))
                pure $ encodeDecode x === Right x

    it "Can encode/decode string[k1][k2]" $ liftEffect do
      quickCheckGen $ do
        k1 <- chooseInt 1 10
        k2 <- chooseInt 1 10
        reifyType k1 \pk1 ->
          reifyType k2 \pk2 -> do
            x <- Vector.generator pk2 (Vector.generator pk1 (arbitrary :: Gen BMPString))
            pure $ encodeDecode x === Right x

  describe "Nested type property tests for array, vector" do

    it "Can encode/decode bytesN[k][]" $ liftEffect do
      for_ bytesSizes $ \n -> do
        quickCheckGen $ do
          k <- chooseInt 1 10
          reifyType k \pk ->
            reifyType n \pn -> do
              x <- arrayOf (Vector.generator pk (BytesN.generator pn))
              pure $ encodeDecode x === Right x

    it "Can encode/decode string[k][]" $ liftEffect do
      quickCheckGen $ do
        k <- chooseInt 1 10
        reifyType k \pk -> do
          x <- arrayOf (Vector.generator pk (arbitrary :: Gen BMPString))
          pure $ encodeDecode x === Right x

  describe "Nested type property tests for vector, array" do

    it "Can encode/decode uintN[][k]" $ liftEffect do
      for_ intSizes $ \n -> do
        quickCheckGen $ do
          k <- chooseInt 1 10
          reifyType k \pk ->
            reifyType n \pn -> do
              x <- (Vector.generator pk (arrayOf $ UIntN.generator pn))
              pure $ encodeDecode x === Right x

    it "Can encode/decode string[][k]" $ liftEffect do
      quickCheckGen $ do
        k <- chooseInt 1 10
        reifyType k \pk -> do
          x <- (Vector.generator pk (arrayOf (arbitrary :: Gen BMPString)))
          pure $ encodeDecode x === Right x

  describe "Nested type property tests for array, array" do

    it "Can encode/decode intN[][]" $ liftEffect do
      for_ intSizes $ \n -> do
        quickCheckGen $
          reifyType n \pn -> do
            x <- (arrayOf (arrayOf $ IntN.generator pn))
            pure $ encodeDecode x === Right x

    it "Can encode/decode string[][]" $ liftEffect do
      quickCheck \(x :: Array (Array BMPString)) ->
        encodeDecode x === Right x

--------------------------------------------------------------------------------
newtype BMPString = BMPString String

derive newtype instance Eq BMPString
derive newtype instance Show BMPString
derive newtype instance ABIDecode BMPString
derive newtype instance ABIEncode BMPString
derive newtype instance EncodingType BMPString

data UnicodeChar = Normal CodePoint | Surrogates CodePoint CodePoint

instance Arbitrary BMPString where
  arbitrary = BMPString <$> do
    ucs <- arrayOf arbitrary
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

intSizes :: NonEmptyArray Int
intSizes = unsafePartial fromJust
  $ fromArray
  $ filter (\x -> x `mod` 8 == 0) (8 .. 256)

bytesSizes :: NonEmptyArray Int
bytesSizes = 1 NEA... 32
