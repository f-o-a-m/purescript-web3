module Network.Ethereum.Web3.Solidity.AbiEncoding
  ( bytesBuilder
  , class ABIDecode
  , fromDataParser
  , class ABIEncode
  , toDataBuilder
  , fromBool
  , fromData
  , int256HexBuilder
  , parseBytes
  , toBool
  , uInt256HexBuilder
  , uInt256HexParser
  ) where

import Prelude

import Data.Array (cons, fold, foldMap, length)
import Data.Array.Partial (init)
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, length) as BS
import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Maybe (fromJust, maybe)
import Data.String (splitAt)
import Data.Traversable (for, scanl)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Network.Ethereum.Core.BigNumber (fromString, fromTwosComplement, toString, toTwosComplement, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, PadByte(..), fromByteString, mkHexString, numberOfBytes, padLeft, padRight, splitAtByteOffset, toByteString, unHex)
import Network.Ethereum.Types (Address, BigNumber, embed, mkAddress, unAddress)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Solidity.Int (IntN, unIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Size (class ByteSize, class IntSize, class KnownSize, sizeVal)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Vector (Vector, unVector)
import Parsing (ParseError, Parser, ParseState(..), Position(..), ParserT, fail, getParserT, stateParserT, runParser)
import Parsing.Combinators (lookAhead)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy(..))

-- | Class representing values that have an encoding and decoding instance to/from a solidity type.
class ABIEncode a where
  toDataBuilder :: a -> HexString

class ABIDecode a where
  fromDataParser :: Parser HexString a

instance abiEncodeAlgebra :: ABIEncode BigNumber where
  toDataBuilder = int256HexBuilder

instance abiDecodeAlgebra :: ABIDecode BigNumber where
  fromDataParser = int256HexParser

-- | Parse encoded value, droping the leading `0x`
fromData :: forall a. ABIDecode a => HexString -> Either ParseError a
fromData = flip runParser fromDataParser

instance abiEncodeBool :: ABIEncode Boolean where
  toDataBuilder = uInt256HexBuilder <<< fromBool

instance abiDecodeBool :: ABIDecode Boolean where
  fromDataParser = toBool <$> uInt256HexParser

instance abiEncodeInt :: ABIEncode Int where
  toDataBuilder = int256HexBuilder <<< embed

instance abiDecodeInt :: ABIDecode Int where
  fromDataParser = unsafeToInt <$> int256HexParser

instance abiEncodeAddress :: ABIEncode Address where
  toDataBuilder addr = padLeft Zero <<< unAddress $ addr

instance abiDecodeAddress :: ABIDecode Address where
  fromDataParser = do
    _ <- parseBytes 12
    maddr <- mkAddress <$> parseBytes 20
    maybe (fail "Address is 20 bytes, receieved more") pure maddr

instance abiEncodeBytesD :: ABIEncode ByteString where
  toDataBuilder bytes = uInt256HexBuilder (embed $ BS.length bytes) <> bytesBuilder bytes

instance abiDecodeBytesD :: ABIDecode ByteString where
  fromDataParser = do
    len <- fromDataParser
    toByteString <$> parseBytes (unsafeToInt len)

instance abiEncodeString :: ABIEncode String where
  toDataBuilder = toDataBuilder <<< BS.toUTF8

instance abiDecodeString :: ABIDecode String where
  fromDataParser = BS.fromUTF8 <$> fromDataParser

instance abiEncodeBytesN :: ByteSize n => ABIEncode (BytesN n) where
  toDataBuilder bs = bytesBuilder <<< unBytesN $ bs

instance abiDecodeBytesN :: ByteSize n => ABIDecode (BytesN n) where
  fromDataParser = do
    let
      len = sizeVal (Proxy :: Proxy n)

      zeroBytes = 32 - len
    raw <- parseBytes len
    _ <- parseBytes zeroBytes
    pure <<< update proxyBytesN <<< toByteString $ raw

instance abiEncodeVec :: (EncodingType a, ABIEncode a, KnownSize n) => ABIEncode (Vector n a) where
  toDataBuilder l =
    if isDynamic (Proxy :: Proxy a) then do
      let
        encs = map toDataBuilder (unVector l)
        lengths = map numberOfBytes encs
        len = sizeVal (Proxy :: Proxy n)
        offsets =
          let
            seed = 32 * len
          in
            seed `cons` (unsafePartial $ init $ scanl (+) seed lengths)
      foldMap toDataBuilder offsets <> fold encs
    else
      foldMap toDataBuilder $ (unVector l :: Array a)

instance abiDecodeVec :: (EncodingType a, KnownSize n, ABIDecode a) => ABIDecode (Vector n a) where
  fromDataParser = do
    let
      len = sizeVal (Proxy :: Proxy n)
    if isDynamic (Proxy :: Proxy a) then do
      offsets <- replicateA len uInt256HexParser
      let
        currentOffset = 32 * len
      for offsets
        $ \dataOffset ->
            lookAhead
              $ do
                  _ <- parseBytes (unsafeToInt dataOffset - currentOffset)
                  fromDataParser
    else
      replicateA len fromDataParser

instance abiEncodeAray :: (EncodingType a, ABIEncode a) => ABIEncode (Array a) where
  toDataBuilder l = do
    uInt256HexBuilder (embed $ length l)
      <>
        if isDynamic (Proxy :: Proxy a) then do
          let
            encs = map toDataBuilder l

            lengths = map numberOfBytes encs

            offsets =
              let
                seed = 32 * length l
              in
                seed `cons` (unsafePartial $ init $ scanl (+) seed lengths)
          foldMap (uInt256HexBuilder <<< embed) offsets <> fold encs
        else
          foldMap toDataBuilder l

instance abiDecodeArray :: (EncodingType a, ABIDecode a) => ABIDecode (Array a) where
  fromDataParser = do
    len <- unsafeToInt <$> uInt256HexParser
    if isDynamic (Proxy :: Proxy a) then do
      offsets <- replicateA len uInt256HexParser
      let
        currentOffset = 32 * len
      for offsets
        $ \dataOffset ->
            lookAhead
              $ do
                  _ <- parseBytes (unsafeToInt dataOffset - currentOffset)
                  fromDataParser
    else
      replicateA len fromDataParser

instance abiEncodeUint :: IntSize n => ABIEncode (UIntN n) where
  toDataBuilder a = uInt256HexBuilder <<< unUIntN $ a

instance abiDecodeUint :: IntSize n => ABIDecode (UIntN n) where
  fromDataParser = do
    a <- uInt256HexParser
    maybe (fail $ msg a) pure <<< uIntNFromBigNumber (Proxy :: Proxy n) $ a
    where
    msg n =
      let
        size = sizeVal (Proxy :: Proxy n)
      in
        "Couldn't parse as uint" <> show size <> " : " <> show n

instance abiEncodeIntN :: IntSize n => ABIEncode (IntN n) where
  toDataBuilder a = int256HexBuilder <<< unIntN $ a

instance abiDecodeIntN :: IntSize n => ABIDecode (IntN n) where
  fromDataParser = do
    a <- int256HexParser
    maybe (fail $ msg a) pure <<< intNFromBigNumber (Proxy :: Proxy n) $ a
    where
    msg n =
      let
        size = sizeVal (Proxy :: Proxy n)
      in
        "Couldn't parse as int" <> show size <> " : " <> show n

instance abiEncodeTagged :: ABIEncode a => ABIEncode (Tagged s a) where
  toDataBuilder = toDataBuilder <<< untagged

instance abiDecodeTagged :: ABIDecode a => ABIDecode (Tagged s a) where
  fromDataParser = tagged <$> fromDataParser

--------------------------------------------------------------------------------
-- | Special Builders and Parsers
--------------------------------------------------------------------------------

-- | base16 encode, then utf8 encode, then pad
bytesBuilder :: ByteString -> HexString
bytesBuilder = padRight Zero <<< fromByteString

-- | Encode something that is essentaially a signed integer.
int256HexBuilder :: BigNumber -> HexString
int256HexBuilder x =
  let
    a = toTwosComplement 256 x
    x' =
      if a < zero then unsafeCrashWith $ "FUCK " <> show a
      else unsafePartial $ fromJust $ mkHexString (toString a)
  in
    if x < zero then padLeft FF x'
    else padLeft Zero x'

-- | Encode something that is essentially an unsigned integer.
uInt256HexBuilder :: BigNumber -> HexString
uInt256HexBuilder x = unsafePartial $ fromJust $
  padLeft Zero <$> mkHexString (toString x)

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
  a <- maybe (fail $ "Failed to parse bytes as BigNumber " <> bs) pure (fromString bs)
  pure a

-- | Decode a `Boolean` as a BigNumber
fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

-- | Encode a `Boolean` as a `BigNumber`
toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

-- | Read any number of HexDigits
parseBytes :: forall m. Monad m => Int -> ParserT HexString m HexString
parseBytes n = do
  ParseState input (Position position) _ <- getParserT
  if numberOfBytes input < n then
    fail "Unexpected EOF"
  else do
    let
      { after, before } = splitAtByteOffset n input

      position' = Position $ position { column = position.column + n }

    let newState = ParseState after position' true
    stateParserT $ const (Tuple before newState)
