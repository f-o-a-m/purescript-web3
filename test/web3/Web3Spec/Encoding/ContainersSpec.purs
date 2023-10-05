module Web3Spec.Encoding.ContainersSpec (spec, BMPString(..)) where

import Prelude

import Control.Monad.Gen (chooseInt, frequency, oneOf, suchThat)
import Data.Array (filter, foldMap, take, (..))
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Enum (toEnumWithDefaults)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.NonEmpty (NonEmpty(..))
import Data.Reflectable (reifyType)
import Data.String (CodePoint, fromCodePointArray)
import Data.Tuple (Tuple(..))
import Debug (trace, traceM)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.Signatures as Address
import Network.Ethereum.Web3.Solidity (class ABIDecodableValue, class ABIEncodableValue, class GenericABIDecode, class GenericABIEncode, Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), abiEncode, abiValueParser)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecodableValue, class ABIEncodableValue, abiDecode, abiEncode, parseABIValue)
import Network.Ethereum.Web3.Solidity.Bytes as BytesN
import Network.Ethereum.Web3.Solidity.Int as IntN
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Network.Ethereum.Web3.Solidity.Vector as Vector
import Parsing (ParseError)
import Partial.Unsafe (unsafeCrashWith)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, quickCheckGen, quickCheckGen', (===))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "encoding-spec for containers" do
    --typePropertyTests
    --arrayTypePropertyTests
    --vecTypePropertyTests
    --nestedTypePropertyTests
    tupleTests

typePropertyTests :: Spec Unit
typePropertyTests =
  describe "Type property tests" do
    it "can encode/decode a string" $ liftEffect $ do
      Console.log "wtf"
      quickCheck \(x :: BMPString) ->
        let
          y = un BMPString x
        in
          (encodeDecode y) === Right y

    it "can encode/decode bytestring" $ liftEffect $ do
      quickCheckGen $ do
        n <- chooseInt 1 100
        x <- Hex.toByteString <$> Hex.generator n
        pure $ encodeDecode x === Right x

    it "can encode/decode bool" $ liftEffect $ do
      quickCheck \(x :: Boolean) -> encodeDecode x === Right x

    it "can encode/decode address" $ liftEffect $ do
      quickCheckGen $ do
        x <- Address.generator
        pure $ encodeDecode x === Right x

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
      quickCheck \(x :: BMPString) ->
        let
          y = un BMPString x
        in
          encodeDecode y === Right y

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
      quickCheckGen $ do
        x <- Address.generator
        pure $ encodeDecode x === Right x

    it "Can encode/decode string[]" $ liftEffect do
      quickCheck $ \(x :: Array BMPString) ->
        let
          y = map (un BMPString) x
        in
          encodeDecode y === Right y

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
          x <- Vector.generator pk Address.generator
          pure $ encodeDecode x === Right x

    it "Can encode/decode string[k]" $ liftEffect do
      quickCheckGen $ do
        k <- chooseInt 1 10
        reifyType k \pk -> do
          _x <- Vector.generator pk (arbitrary :: Gen BMPString)
          let x = un BMPString <$> _x
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
            _x <- Vector.generator pk2 (Vector.generator pk1 (arbitrary :: Gen BMPString))
            let x = map (un BMPString) <$> _x
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
          _x <- arrayOf (Vector.generator pk (arbitrary :: Gen BMPString))
          let x = map (un BMPString) <$> _x
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
          _x <- (Vector.generator pk (arrayOf (arbitrary :: Gen BMPString)))
          let x = map (un BMPString) <$> _x
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
        let
          y = map (map (un BMPString)) x
        in
          encodeDecode y === Right y

tupleTests :: Spec Unit
tupleTests = do

  describe "Basic static sized Tuple Tests" $ do

    it "Can encode/decode (intN, address, bool, uintN, bytesN)" $ liftEffect do
      quickCheckGen $ do
        n <- oneOf (pure <$> intSizes)
        m <- oneOf (pure <$> intSizes)
        k <- oneOf (pure <$> bytesSizes)
        reifyType n \pn ->
          reifyType m \pm ->
            reifyType k \pk -> do
              int <- IntN.generator pn
              addr <- Address.generator
              bool <- arbitrary :: Gen Boolean
              uint <- UIntN.generator pm
              bytes <- BytesN.generator pk
              let x = Tuple5 int addr bool uint bytes
              pure $ encodeDecode x === Right x

    it "Can encode/decode (address[k], bool, intN[k], uint)" $ liftEffect do
      quickCheckGen' 1 $ do
        k1 <- chooseInt 1 10
        k2 <- chooseInt 1 10
        n <- oneOf (pure <$> intSizes)
        m <- oneOf (pure <$> intSizes)
        reifyType k1 \pk1 ->
          reifyType k2 \pk2 ->
            reifyType n \pn -> do
              reifyType m \pm -> do
                addrs <- arrayOf (Vector.generator pk1 Address.generator)
                bool <- arbitrary @Boolean
                ints <- Vector.generator pk2 (IntN.generator pn)
                uint <- (UIntN.generator pm)
                let x = Tuple4 addrs bool ints uint
                pure $ encodeDecode x === Right x

  describe "Basic dynamic sized Tuple Tests" $ do

    it "Can encode/decode (intN[], bytes, address[][k], string[k][], bool)" $ liftEffect do
      quickCheckGen $ do
        n <- oneOf (pure <$> intSizes)
        m <- chooseInt 1 10
        k <- chooseInt 1 10
        reifyType n \pn ->
          reifyType m \pm ->
            reifyType k \pk -> do
              ints <- arrayOf (IntN.generator pn)
              bytes <- Hex.toByteString <$> (chooseInt 1 100 >>= Hex.generator)
              addrs <- Vector.generator pm (arrayOf Address.generator)
              strings <- arrayOf (Vector.generator pk (arbitrary @BMPString))
              bool <- arbitrary :: Gen Boolean
              let x = Tuple5 ints bytes addrs (map (un BMPString) <$> strings) bool
              pure $ encodeDecode x === Right x

    it "Can encode/decode (address[k], bool, intN[k], uint)" $ liftEffect do
      quickCheckGen' 5 $ do
        k1 <- chooseInt 1 10
        k2 <- chooseInt 1 10
        n <- oneOf (pure <$> intSizes)
        m <- oneOf (pure <$> intSizes)
        reifyType k1 \pk1 ->
          reifyType k2 \pk2 ->
            reifyType n \pn -> do
              reifyType m \pm -> do
                addrs <- arrayOf (Vector.generator pk1 Address.generator)
                bool <- arbitrary @Boolean
                ints <- Vector.generator pk2 (IntN.generator pn)
                uint <- (UIntN.generator pm)
                let x = Tuple4 addrs bool ints uint
                pure $ encodeDecode x === Right x

    it "Can encode/decode arrays of tuples" $ liftEffect do
      quickCheckGen' 5 $ do
        k1 <- chooseInt 1 3
        reifyType k1 \pk1 ->
          do
            let
              tupleGen = do
                addrs <- arrayOf (Vector.generator pk1 Address.generator)
                bool <- arbitrary @Boolean
                pure $ Tuple2 addrs bool
            as <- take 2 <$> arrayOf tupleGen
            traceM ("as: " <> show (abiEncode as))
            pure $ encodeDecode as === Right as

    -- this test is admittedly pretty ad hoc
    it "Can encode/decode nested tuples" $ liftEffect do
      quickCheckGen' 5 do
        k1 <- chooseInt 1 10
        k2 <- chooseInt 1 10
        n <- oneOf (pure <$> intSizes)
        m <- oneOf (pure <$> intSizes)
        reifyType k1 \pk1 ->
          reifyType k2 \pk2 ->
            reifyType n \pn ->
              reifyType m \pm -> do
                let
                  mkTuple4 = do
                    addrs <- arrayOf (Vector.generator pk1 Address.generator)
                    bool <- arbitrary @Boolean
                    ints <- Vector.generator pk2 (IntN.generator pn)
                    uint <- (UIntN.generator pm)
                    pure $ Tuple4 addrs bool ints uint
                _n <- oneOf (pure <$> intSizes)
                _m <- chooseInt 1 10
                _k <- chooseInt 1 10
                reifyType _n \_pn ->
                  reifyType _m \_pm ->
                    reifyType _k \_pk -> do
                      let
                        mkTuple5 = do
                          ints <- arrayOf (IntN.generator _pn)
                          bytes <- Hex.toByteString <$> (chooseInt 1 100 >>= Hex.generator)
                          addrs <- Vector.generator _pm (arrayOf Address.generator)
                          strings <- map (map (un BMPString)) <$>
                            arrayOf (Vector.generator _pk (arbitrary @BMPString))
                          bool <- arbitrary :: Gen Boolean
                          pure $ Tuple5 ints bytes addrs strings bool
                        mkTuple2 = do
                          strings <- map (un BMPString) <$>
                            arrayOf (arbitrary @BMPString)
                          addrs <- Vector.generator pk2 (arrayOf Address.generator)
                          pure $ Tuple2 strings addrs

                      t <- Tuple3 <$> mkTuple5 <*> mkTuple4 <*> mkTuple2
                      pure $ encodeDecode t === Right t

--------------------------------------------------------------------------------
newtype BMPString = BMPString String

derive newtype instance Eq BMPString
derive newtype instance Show BMPString

derive instance Newtype BMPString _

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
  => ABIEncodableValue a
  => ABIDecodableValue a
  => a
  -> Either ParseError a
encodeDecode x =
  let
    a = abiEncode x
  in
    --trace ("encoded: " <> show a) $ \_ ->
    parseABIValue a

intSizes :: NonEmptyArray Int
intSizes = case fromArray $ filter (\x -> x `mod` 8 == 0) (8 .. 256) of
  Nothing -> unsafeCrashWith "intSizes: impossible"
  Just x -> x

bytesSizes :: NonEmptyArray Int
bytesSizes = 1 NEA... 32
