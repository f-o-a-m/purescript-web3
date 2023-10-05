module Network.Ethereum.Web3.Solidity.AbiEncoding
  ( class ABIDecode
  , abiDecode
  , _abiDecode
  , class ABIEncode
  , abiEncode
  , class EncodingType
  , isDynamic
  , class GEncodingType
  , gIsDynamic
  , class GenericABIDecode
  , gABIDecode
  , class GenericABIEncode
  , gAbiEncode
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
class EncodingType a <= ABIEncode a where
  abiEncode :: a -> HexString

instance ABIEncode BigNumber where
  abiEncode = int256HexBuilder

else instance ABIEncode Boolean where
  abiEncode b = uInt256HexBuilder $ if b then one else zero

else instance ABIEncode Int where
  abiEncode = int256HexBuilder <<< fromInt

else instance Reflectable n Int => ABIEncode (UIntN n) where
  abiEncode a = uInt256HexBuilder <<< unUIntN $ a

else instance ABIEncode Address where
  abiEncode addr = padLeft Zero <<< unAddress $ addr

else instance Reflectable n Int => ABIEncode (BytesN n) where
  abiEncode bs = bytesBuilder <<< unBytesN $ bs

else instance Reflectable n Int => ABIEncode (IntN n) where
  abiEncode a = int256HexBuilder <<< unIntN $ a

else instance ABIEncode ByteString where
  abiEncode bytes = uInt256HexBuilder (fromInt $ BS.length bytes) <> bytesBuilder bytes

else instance ABIEncode String where
  abiEncode = abiEncode <<< BS.toUTF8

else instance ABIEncode a => ABIEncode (Array a) where
  abiEncode l =
    uInt256HexBuilder (fromInt $ length l) <>
      (combineEncodedValues $ un Endo (foldMapDefaultR factorBuilder l) [])

else instance (ABIEncode a, Reflectable n Int) => ABIEncode (Vector n a) where
  abiEncode l =
    combineEncodedValues $ un Endo (foldMapDefaultR factorBuilder $ unVector l) []

else instance ABIEncode a => ABIEncode (Identity a) where
  abiEncode = abiEncode <<< un Identity

else instance ABIEncode a => ABIEncode (Tagged s a) where
  abiEncode = abiEncode <<< untagged

else instance (Generic a rep, EncodingType a, GenericABIEncode rep) => ABIEncode a where
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

else instance ABIEncode b => GenericABIEncode (Argument b) where
  gAbiEncode (Argument b) = factorBuilder b

else instance (GenericABIEncode a, GenericABIEncode b) => GenericABIEncode (Product a b) where
  gAbiEncode (Product a b) = gAbiEncode a <> gAbiEncode b

factorBuilder :: forall a. ABIEncode a => a -> ABIDataBuilder
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

abiDecode :: forall a. ABIDecode a => HexString -> Either ParseError a
abiDecode = flip runParser _abiDecode

class EncodingType a <= ABIDecode a where
  _abiDecode :: Parser HexString a

instance ABIDecode BigNumber where
  _abiDecode = int256HexParser

else instance ABIDecode Boolean where
  _abiDecode = toBool <$> uInt256HexParser
    where
    toBool bn = not $ bn == zero

else instance ABIDecode Int where
  _abiDecode = unsafeToInt <$> int256HexParser

else instance ABIDecode Address where
  _abiDecode = do
    _ <- parseBytes 12
    maddr <- mkAddress <$> parseBytes 20
    maybe (fail "Address is 20 bytes, receieved more") pure maddr

else instance ABIDecode ByteString where
  _abiDecode = do
    len <- _abiDecode
    toByteString <$> parseBytes len

else instance ABIDecode String where
  _abiDecode = BS.fromUTF8 <$> _abiDecode

else instance Reflectable n Int => ABIDecode (BytesN n) where
  _abiDecode = do
    let
      len = reflectType (Proxy :: Proxy n)
      zeroBytes = 32 - len
    raw <- parseBytes len
    _ <- parseBytes zeroBytes
    pure <<< update proxyBytesN <<< toByteString $ raw

else instance (Reflectable n Int, ABIDecode a) => ABIDecode (Vector n a) where
  _abiDecode =
    let
      len = reflectType (Proxy :: Proxy n)
    in
      replicateA len factorParser

else instance ABIDecode a => ABIDecode (Array a) where
  _abiDecode = do
    len <- _abiDecode
    resetOffset
    replicateA len factorParser

else instance Reflectable n Int => ABIDecode (UIntN n) where
  _abiDecode = do
    a <- uInt256HexParser
    maybe (fail $ msg a) pure <<< uIntNFromBigNumber (Proxy @n) $ a
    where
    msg n =
      let
        size = reflectType (Proxy @n)
      in
        "Couldn't parse as uint" <> show size <> " : " <> show n

else instance Reflectable n Int => ABIDecode (IntN n) where
  _abiDecode = do
    a <- int256HexParser
    maybe (fail $ msg a) pure <<< intNFromBigNumber (Proxy :: Proxy n) $ a
    where
    msg n =
      let
        size = reflectType (Proxy :: Proxy n)
      in
        "Couldn't parse as int" <> show size <> " : " <> show n

else instance ABIDecode a => ABIDecode (Tagged s a) where
  _abiDecode = tagged <$> _abiDecode

else instance ABIDecode a => ABIDecode (Identity a) where
  _abiDecode = Identity <$> _abiDecode

else instance (Generic a rep, EncodingType a, GenericABIDecode rep) => ABIDecode a where
  _abiDecode = to <$> gABIDecode

class GenericABIDecode a where
  gABIDecode :: Parser HexString a

instance GenericABIDecode NoArguments where
  gABIDecode = pure NoArguments

else instance ABIDecode a => GenericABIDecode (Argument a) where
  gABIDecode = Argument <$> factorParser

else instance (IsSymbol name, GenericABIDecode a) => GenericABIDecode (Constructor name a) where
  gABIDecode = Constructor <$> gABIDecode

else instance (GenericABIDecode b, GenericABIDecode a) => GenericABIDecode (Product a b) where
  gABIDecode = Product <$> gABIDecode <*> gABIDecode

factorParser :: forall a. ABIDecode a => Parser HexString a
factorParser
  | isDynamic (Proxy :: Proxy a) = do
      dataOffset <- _abiDecode
      found <- lookAhead
        $ do
            (ParseState _ (Position { index }) _) <- getParserT
            void $ parseBytes (dataOffset - index)
            resetOffset
            _abiDecode
      pure found
  | otherwise = _abiDecode

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
