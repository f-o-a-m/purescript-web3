module Network.Ethereum.Web3.Solidity.AbiEncoding
  ( class ABIDecodableValue
  , abiValueParser
  , class ABIEncodableValue
  , abiEncode
  , parseABIValue
  , class GenericABIDecode
  , gABIDecode
  , abiDecode
  , class GenericABIEncode
  , gAbiEncode
  , class EncodingType
  , isDynamic
  , class GEncodingType
  , gIsDynamic
  ) where

import Prelude

import Data.Array (foldMap, foldl, length, sortBy, (:))
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, length) as BS
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, repOf, to)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (un)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol)
import Data.Traversable (foldMapDefaultR)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Network.Ethereum.Core.BigNumber (fromString, fromTwosComplement, toString, toTwosComplement, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, PadByte(..), fromByteString, mkHexString, numberOfBytes, padLeft, padRight, splitAtByteOffset, toByteString, unHex)
import Network.Ethereum.Types (Address, BigNumber, fromInt, mkAddress, unAddress)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Solidity.Int (IntN, unIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Vector (Vector, unVector)
import Parsing (ParseError, ParseState(..), Parser, ParserT, Position(..), fail, getParserT, runParser, stateParserT)
import Parsing.Combinators (lookAhead)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

class EncodingType :: forall k. k -> Constraint
class EncodingType a where
  isDynamic :: Proxy a -> Boolean

instance EncodingType Boolean where
  isDynamic = const false
else instance EncodingType Int where
  isDynamic = const false
else instance EncodingType BigNumber where
  isDynamic = const false
else instance EncodingType (UIntN n) where
  isDynamic = const false
else instance EncodingType (IntN n) where
  isDynamic = const false
else instance EncodingType String where
  isDynamic = const true
else instance EncodingType Address where
  isDynamic = const false
else instance EncodingType a => EncodingType (Array a) where
  isDynamic = const true
else instance EncodingType (BytesN n) where
  isDynamic = const false
else instance EncodingType a => EncodingType (Vector n a) where
  isDynamic _ = isDynamic (Proxy :: Proxy a)
else instance EncodingType ByteString where
  isDynamic = const true
else instance EncodingType a => EncodingType (Tagged s a) where
  isDynamic _ = isDynamic (Proxy :: Proxy a)
else instance EncodingType a => EncodingType (Identity a) where
  isDynamic _ = isDynamic (Proxy :: Proxy a)
else instance (Generic a rep, GEncodingType rep) => EncodingType a where
  isDynamic p = gIsDynamic (repOf p)

class GEncodingType :: forall k. k -> Constraint
class GEncodingType rep where
  gIsDynamic :: Proxy rep -> Boolean

instance GEncodingType NoArguments where
  gIsDynamic _ = false
else instance EncodingType a => GEncodingType (Argument a) where
  gIsDynamic _ = isDynamic (Proxy @a)
else instance (GEncodingType a, GEncodingType b) => GEncodingType (Product a b) where
  gIsDynamic _ = gIsDynamic (Proxy @a) || gIsDynamic (Proxy @b)
else instance GEncodingType a => GEncodingType (Constructor s a) where
  gIsDynamic _ = gIsDynamic (Proxy @a)

-- | Class representing values that have an encoding and decoding instance to/from a solidity type.
class EncodingType a <= ABIEncodableValue a where
  abiEncode :: a -> HexString

instance ABIEncodableValue BigNumber where
  abiEncode = int256HexBuilder

else instance ABIEncodableValue Boolean where
  abiEncode b = uInt256HexBuilder $ if b then one else zero

else instance ABIEncodableValue Int where
  abiEncode = int256HexBuilder <<< fromInt

else instance Reflectable n Int => ABIEncodableValue (UIntN n) where
  abiEncode a = uInt256HexBuilder <<< unUIntN $ a

else instance ABIEncodableValue Address where
  abiEncode addr = padLeft Zero <<< unAddress $ addr

else instance Reflectable n Int => ABIEncodableValue (BytesN n) where
  abiEncode bs = bytesBuilder <<< unBytesN $ bs

else instance Reflectable n Int => ABIEncodableValue (IntN n) where
  abiEncode a = int256HexBuilder <<< unIntN $ a

else instance ABIEncodableValue ByteString where
  abiEncode bytes = uInt256HexBuilder (fromInt $ BS.length bytes) <> bytesBuilder bytes

else instance ABIEncodableValue String where
  abiEncode = abiEncode <<< BS.toUTF8

else instance ABIEncodableValue a => ABIEncodableValue (Array a) where
  abiEncode l =
    uInt256HexBuilder (fromInt $ length l) <>
      (combineEncodedValues $ un Endo (foldMapDefaultR factorBuilder l) [])

else instance (ABIEncodableValue a, Reflectable n Int) => ABIEncodableValue (Vector n a) where
  abiEncode l =
    combineEncodedValues $ un Endo (foldMapDefaultR factorBuilder $ unVector l) []

else instance ABIEncodableValue a => ABIEncodableValue (Identity a) where
  abiEncode = abiEncode <<< un Identity

else instance ABIEncodableValue a => ABIEncodableValue (Tagged s a) where
  abiEncode = abiEncode <<< untagged

else instance (Generic a rep, EncodingType a, GenericABIEncode rep) => ABIEncodableValue a where
  abiEncode a = combineEncodedValues $ un Endo (gAbiEncode $ from a) []

type EncodedValue =
  { order :: Int
  , isDynamic :: Boolean
  , encoding :: HexString
  , encodingLengthInBytes :: Int -- cache
  }

type ABIDataBuilder = Endo (->) (Array EncodedValue)

-- | An internally used class for encoding
class GenericABIEncode rep where
  gAbiEncode :: rep -> ABIDataBuilder

combineEncodedValues :: Array EncodedValue -> HexString
combineEncodedValues =
  sortBy (\_a _b -> _a.order `compare` _b.order)
    >>> \encodings ->
      let
        wordLengthInBytes = 32

        headsOffsetInBytes :: Int
        headsOffsetInBytes =
          let
            f = \encodedValueSimple ->
              if encodedValueSimple.isDynamic then wordLengthInBytes
              else encodedValueSimple.encodingLengthInBytes
          in
            foldl (+) 0 $ map f encodings

        (heads :: HexString) =
          foldl
            ( \{ accumulator, lengthOfPreviousDynamicValues } encodedValue ->
                if encodedValue.isDynamic then
                  { accumulator: accumulator <> uInt256HexBuilder (fromInt $ headsOffsetInBytes + lengthOfPreviousDynamicValues)
                  , lengthOfPreviousDynamicValues: lengthOfPreviousDynamicValues + encodedValue.encodingLengthInBytes
                  }
                else
                  { accumulator: accumulator <> encodedValue.encoding
                  , lengthOfPreviousDynamicValues: lengthOfPreviousDynamicValues
                  }
            )
            { accumulator: mempty
            , lengthOfPreviousDynamicValues: 0
            }
            encodings
            # _.accumulator

        (tails :: HexString) =
          foldMap
            ( \encodedValue ->
                if encodedValue.isDynamic then
                  encodedValue.encoding
                else
                  mempty
            )
            encodings
      in
        heads <> tails

instance GenericABIEncode NoArguments where
  gAbiEncode _ = mempty

else instance GenericABIEncode b => GenericABIEncode (Constructor s b) where
  gAbiEncode (Constructor b) = gAbiEncode b

else instance ABIEncodableValue b => GenericABIEncode (Argument b) where
  gAbiEncode (Argument b) = factorBuilder b

else instance (GenericABIEncode a, GenericABIEncode b) => GenericABIEncode (Product a b) where
  gAbiEncode (Product a b) = gAbiEncode a <> gAbiEncode b

factorBuilder :: forall a. ABIEncodableValue a => a -> ABIDataBuilder
factorBuilder a = Endo \encoded ->
  let
    encoding = abiEncode a
  in
    { encoding
    , order: 1
    , isDynamic: isDynamic (Proxy :: Proxy a)
    , encodingLengthInBytes: numberOfBytes encoding
    } : map (\x -> x { order = x.order + 1 }) encoded

-- | base16 encode, then utf8 encode, then pad
bytesBuilder :: ByteString -> HexString
bytesBuilder = padRight Zero <<< fromByteString

-- | Encode something that is essentaially a signed integer.
int256HexBuilder :: BigNumber -> HexString
int256HexBuilder x =
  let
    x' = case mkHexString $ toString $ toTwosComplement 256 x of
      Nothing -> unsafeCrashWith $ "Failed to encode as hex string: " <> show x
      Just a -> a
  in
    if x < zero then padLeft FF x'
    else padLeft Zero x'

-- | Encode something that is essentially an unsigned integer.
uInt256HexBuilder :: BigNumber -> HexString
uInt256HexBuilder x =
  case padLeft Zero <$> mkHexString (toString x) of
    Nothing -> unsafeCrashWith $ "Failed to encode as hex string: " <> show x
    Just a -> a

--------------------------------------------------------------------------------

class EncodingType a <= ABIDecodableValue a where
  abiValueParser :: Parser HexString a

parseABIValue :: forall a. ABIDecodableValue a => HexString -> Either ParseError a
parseABIValue = flip runParser abiValueParser

instance ABIDecodableValue BigNumber where
  abiValueParser = int256HexParser

else instance ABIDecodableValue Boolean where
  abiValueParser = toBool <$> uInt256HexParser
    where
    toBool bn = not $ bn == zero

else instance ABIDecodableValue Int where
  abiValueParser = unsafeToInt <$> int256HexParser

else instance ABIDecodableValue Address where
  abiValueParser = do
    _ <- parseBytes 12
    maddr <- mkAddress <$> parseBytes 20
    maybe (fail "Address is 20 bytes, receieved more") pure maddr

else instance ABIDecodableValue ByteString where
  abiValueParser = do
    len <- abiValueParser
    toByteString <$> parseBytes len

else instance ABIDecodableValue String where
  abiValueParser = BS.fromUTF8 <$> abiValueParser

else instance Reflectable n Int => ABIDecodableValue (BytesN n) where
  abiValueParser = do
    let
      len = reflectType (Proxy :: Proxy n)
      zeroBytes = 32 - len
    raw <- parseBytes len
    _ <- parseBytes zeroBytes
    pure <<< update proxyBytesN <<< toByteString $ raw

else instance (Reflectable n Int, ABIDecodableValue a) => ABIDecodableValue (Vector n a) where
  abiValueParser =
    let
      len = reflectType (Proxy :: Proxy n)
    in
      replicateA len factorParser

else instance ABIDecodableValue a => ABIDecodableValue (Array a) where
  abiValueParser = do
    len <- abiValueParser
    resetOffset
    replicateA len factorParser

else instance Reflectable n Int => ABIDecodableValue (UIntN n) where
  abiValueParser = do
    a <- uInt256HexParser
    maybe (fail $ msg a) pure <<< uIntNFromBigNumber (Proxy @n) $ a
    where
    msg n =
      let
        size = reflectType (Proxy @n)
      in
        "Couldn't parse as uint" <> show size <> " : " <> show n

else instance Reflectable n Int => ABIDecodableValue (IntN n) where
  abiValueParser = do
    a <- int256HexParser
    maybe (fail $ msg a) pure <<< intNFromBigNumber (Proxy :: Proxy n) $ a
    where
    msg n =
      let
        size = reflectType (Proxy :: Proxy n)
      in
        "Couldn't parse as int" <> show size <> " : " <> show n

else instance ABIDecodableValue a => ABIDecodableValue (Tagged s a) where
  abiValueParser = tagged <$> abiValueParser

else instance ABIDecodableValue a => ABIDecodableValue (Identity a) where
  abiValueParser = Identity <$> abiValueParser

else instance (Generic a rep, EncodingType a, GenericABIDecode rep) => ABIDecodableValue a where
  abiValueParser = to <$> gABIDecode

class GenericABIDecode a where
  gABIDecode :: Parser HexString a

abiDecode
  :: forall a
   . ABIDecodableValue a
  => HexString
  -> Either ParseError a
abiDecode hex = runParser hex abiValueParser

instance GenericABIDecode NoArguments where
  gABIDecode = pure NoArguments

else instance ABIDecodableValue a => GenericABIDecode (Argument a) where
  gABIDecode = Argument <$> factorParser

else instance (IsSymbol name, GenericABIDecode a) => GenericABIDecode (Constructor name a) where
  gABIDecode = Constructor <$> gABIDecode

else instance (GenericABIDecode b, GenericABIDecode a) => GenericABIDecode (Product a b) where
  gABIDecode = Product <$> gABIDecode <*> gABIDecode

factorParser :: forall a. ABIDecodableValue a => Parser HexString a
factorParser
  | isDynamic (Proxy :: Proxy a) = do
      dataOffset <- abiValueParser
      found <- lookAhead
        $ do
            (ParseState _ (Position { index }) _) <- getParserT
            void $ parseBytes (dataOffset - index)
            resetOffset
            abiValueParser
      pure found
  | otherwise = abiValueParser

-- | Parse as a signed `BigNumber`
int256HexParser :: forall m. Monad m => ParserT HexString m BigNumber
int256HexParser = do
  bs <- unHex <$> parseBytes 32
  a <- maybe (fail $ "Failed to parse bytes as BigNumber " <> bs) pure (fromString bs)
  pure $ fromTwosComplement 256 a

-- | Parse an unsigned `BigNumber`
uInt256HexParser :: forall m. Monad m => ParserT HexString m BigNumber
uInt256HexParser = do
  bs <- unHex <$> parseBytes 32
  maybe (fail $ "Failed to parse bytes as BigNumber " <> bs) pure (fromString bs)

-- | Read any number of HexDigits
parseBytes :: forall m. Monad m => Int -> ParserT HexString m HexString
parseBytes n
  | n < 0 = fail "Cannot parse negative bytes"
  | n == 0 = pure mempty
  | otherwise = do
      ParseState input (Position position) _ <- getParserT
      when (numberOfBytes input < n) $ fail "Unexpected EOF"
      let
        { after, before } = splitAtByteOffset n input
        position' = Position $ position { index = position.index + n }
        newState = ParseState after position' true
      stateParserT $ const (Tuple before newState)

resetOffset :: forall m. Monad m => ParserT HexString m Unit
resetOffset = stateParserT \(ParseState s (Position p) c) ->
  Tuple unit (ParseState s (Position p { index = 0 }) c)
