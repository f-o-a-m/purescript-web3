module Network.Ethereum.Web3.Solidity.AbiEncoding
  ( class ABIDecodableValue
  , abiValueParser
  , class ABIEncodableValue
  , encodeABIValue
  , parseABIValue
  , class GenericABIDecode
  , gABIDecode
  , abiDecode
  , class GenericABIEncode
  , gAbiEncode
  , abiEncode
  , class EncodingType
  , isDynamic
  , class GEncodingType
  , gIsDynamic
  ) where

import Prelude

import Data.Array (cons, fold, foldMap, foldl, init, length, sortBy, (:))
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, length) as BS
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, repOf, to)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (un)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol)
import Data.Traversable (for, scanl)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Network.Ethereum.Core.BigNumber (fromString, fromTwosComplement, toString, toTwosComplement, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, PadByte(..), fromByteString, mkHexString, numberOfBytes, padLeft, padRight, splitAtByteOffset, toByteString, unHex)
import Network.Ethereum.Types (Address, BigNumber, fromInt, mkAddress, unAddress)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Solidity.Int (IntN, unIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Vector (Vector, unVector, vectorLength)
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
else instance (Generic a rep, GEncodingType rep) => EncodingType a where
  isDynamic p = gIsDynamic (repOf p)

class GEncodingType :: forall k. k -> Constraint
class GEncodingType rep where
  gIsDynamic :: Proxy rep -> Boolean

instance EncodingType a => GEncodingType (Argument a) where
  gIsDynamic _ = isDynamic (Proxy @a)
else instance (GEncodingType a, GEncodingType b) => GEncodingType (Product a b) where
  gIsDynamic _ = gIsDynamic (Proxy @a) || gIsDynamic (Proxy @b)
else instance GEncodingType a => GEncodingType (Constructor s a) where
  gIsDynamic _ = gIsDynamic (Proxy @a)

-- | Class representing values that have an encoding and decoding instance to/from a solidity type.
class EncodingType a <= ABIEncodableValue a where
  encodeABIValue :: a -> HexString

instance ABIEncodableValue BigNumber where
  encodeABIValue = int256HexBuilder

else instance ABIEncodableValue Boolean where
  encodeABIValue b = uInt256HexBuilder $ if b then one else zero

else instance ABIEncodableValue Int where
  encodeABIValue = int256HexBuilder <<< fromInt

else instance Reflectable n Int => ABIEncodableValue (UIntN n) where
  encodeABIValue a = uInt256HexBuilder <<< unUIntN $ a

else instance ABIEncodableValue Address where
  encodeABIValue addr = padLeft Zero <<< unAddress $ addr

else instance Reflectable n Int => ABIEncodableValue (BytesN n) where
  encodeABIValue bs = bytesBuilder <<< unBytesN $ bs

else instance Reflectable n Int => ABIEncodableValue (IntN n) where
  encodeABIValue a = int256HexBuilder <<< unIntN $ a

else instance ABIEncodableValue ByteString where
  encodeABIValue bytes = uInt256HexBuilder (fromInt $ BS.length bytes) <> bytesBuilder bytes

else instance ABIEncodableValue String where
  encodeABIValue = encodeABIValue <<< BS.toUTF8

else instance ABIEncodableValue a => ABIEncodableValue (Array a) where
  encodeABIValue l
    | length l == 0 = uInt256HexBuilder zero
    | otherwise =
        uInt256HexBuilder (fromInt $ length l)
          <>
            if isDynamic (Proxy :: Proxy a) then do
              let
                encs = map encodeABIValue l

                lengths = map numberOfBytes encs

                offsets =
                  let
                    seed = 32 * length l
                    rest =
                      case init $ scanl (+) seed lengths of
                        Nothing -> unsafeCrashWith "Failed to encode Array"
                        Just a -> a
                  in
                    seed `cons` rest
              foldMap (uInt256HexBuilder <<< fromInt) offsets <> fold encs
            else
              foldMap encodeABIValue l

else instance (ABIEncodableValue a, Reflectable n Int) => ABIEncodableValue (Vector n a) where
  encodeABIValue l
    | vectorLength l == 0 = uInt256HexBuilder zero
    | otherwise =
        if isDynamic (Proxy :: Proxy a) then do
          let
            encs = map encodeABIValue (unVector l)
            lengths = map numberOfBytes encs
            len = reflectType (Proxy :: Proxy n)
            offsets =
              let
                seed = 32 * len
                rest =
                  case init $ scanl (+) seed lengths of
                    Nothing -> unsafeCrashWith $ "Failed to encode Vector " <> show len
                    Just a -> a
              in
                seed `cons` rest
          foldMap encodeABIValue offsets <> fold encs
        else
          foldMap encodeABIValue $ (unVector l :: Array a)

else instance ABIEncodableValue a => ABIEncodableValue (Tagged s a) where
  encodeABIValue = encodeABIValue <<< untagged

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

abiEncode
  :: forall a rep
   . Generic a rep
  => GenericABIEncode rep
  => a
  -> HexString
abiEncode a = combineEncodedValues $ un Endo (gAbiEncode $ from a) []

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
    encoding = encodeABIValue a
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
    toByteString <$> parseBytes (unsafeToInt len)

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
  abiValueParser = do
    let
      len = reflectType (Proxy :: Proxy n)
    if isDynamic (Proxy :: Proxy a) then do
      offsets <- replicateA len uInt256HexParser
      let
        currentOffset = 32 * len
      for offsets $ \dataOffset -> lookAhead do
        _ <- parseBytes (unsafeToInt dataOffset - currentOffset)
        abiValueParser
    else
      replicateA len abiValueParser

else instance ABIDecodableValue a => ABIDecodableValue (Array a) where
  abiValueParser = do
    len <- unsafeToInt <$> uInt256HexParser
    if isDynamic (Proxy :: Proxy a) then do
      offsets <- replicateA len uInt256HexParser
      let
        currentOffset = 32 * len
      for offsets $ \dataOffset -> lookAhead $ do
        _ <- parseBytes (unsafeToInt dataOffset - currentOffset)
        abiValueParser
    else
      replicateA len abiValueParser

else instance Reflectable n Int => ABIDecodableValue (UIntN n) where
  abiValueParser = do
    a <- uInt256HexParser
    maybe (fail $ msg a) pure <<< uIntNFromBigNumber (Proxy :: Proxy n) $ a
    where
    msg n =
      let
        size = reflectType (Proxy :: Proxy n)
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

class GenericABIDecode a where
  gABIDecode :: Parser HexString a

abiDecode
  :: forall a rep
   . Generic a rep
  => GenericABIDecode rep
  => HexString
  -> Either ParseError a
abiDecode hex = to <$> runParser hex gABIDecode

instance GenericABIDecode NoArguments where
  gABIDecode = pure NoArguments

else instance ABIDecodableValue a => GenericABIDecode (Argument a) where
  gABIDecode = Argument <$> factorParser
    where
    factorParser
      | isDynamic (Proxy :: Proxy a) = do
          dataOffset <- unsafeToInt <$> uInt256HexParser
          lookAhead
            $ do
                (ParseState _ (Position { index }) _) <- getParserT
                _ <- parseBytes (dataOffset - index)
                stateParserT \(ParseState s (Position p) c) ->
                  Tuple unit (ParseState s (Position p { index = 0 }) c)
                abiValueParser
      | otherwise = abiValueParser

else instance (IsSymbol name, GenericABIDecode a) => GenericABIDecode (Constructor name a) where
  gABIDecode = Constructor <$> gABIDecode

else instance (GenericABIDecode b, GenericABIDecode a) => GenericABIDecode (Product a b) where
  gABIDecode = Product <$> gABIDecode <*> gABIDecode

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
parseBytes n = do
  when (n < 0) $ fail "Cannot parse negative bytes"
  ParseState input (Position position) _ <- getParserT
  when (numberOfBytes input < n) $ fail "Unexpected EOF"
  let
    { after, before } = splitAtByteOffset n input
    position' = Position $ position { index = position.index + n }
    newState = ParseState after position' true
  stateParserT $ const (Tuple before newState)
