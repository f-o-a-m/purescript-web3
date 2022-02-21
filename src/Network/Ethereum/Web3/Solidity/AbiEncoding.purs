module Network.Ethereum.Web3.Solidity.AbiEncoding where

import Prelude

import Control.Monad.State (get, put)
import Data.Array (length, fold) as A
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, toString, fromString, length, Encoding(Hex)) as BS
import Data.Either (Either)
import Data.Foldable (foldMap)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Maybe (maybe, fromJust)
import Data.String (splitAt)
import Data.Unfoldable (replicateA)
import Network.Ethereum.Core.BigNumber (negativeBigNumberToTwosComplement, unsafeToInt)
import Network.Ethereum.Core.HexString (HexString, Signed(..), mkHexString, padLeft, padLeftSigned, padRight, toBigNumberFromSignedHexString, toBigNumber, toSignedHexString, unHex, hexLength, toHexString)
import Network.Ethereum.Types (Address, BigNumber, embed, mkAddress, unAddress)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Solidity.Int (IntN, unIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Size (class ByteSize, class IntSize, class KnownSize, sizeVal)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Vector (Vector)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (ParseError, ParseState(..), Parser, ParserT, fail, position, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))
import Text.Parsing.Parser.Token (hexDigit)
import Data.String.NonEmpty (NonEmptyString)


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
  toDataBuilder addr = padLeft <<< unAddress $ addr

instance abiDecodeAddress :: ABIDecode Address where
  fromDataParser = do
    _ <- takeBytes 12
    maddr <- mkAddress <$> takeBytes 20
    maybe (fail "Address is 20 bytes, receieved more") pure maddr

instance abiEncodeBytesD :: ABIEncode ByteString where
  toDataBuilder bytes = uInt256HexBuilder (embed $ BS.length bytes) <> bytesBuilder bytes

instance abiDecodeBytesD :: ABIDecode ByteString where
  fromDataParser = do
    len <- unsafeToInt <$> fromDataParser
    bytesDecode <<< unHex <$> takeBytes len

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
    raw <- takeBytes len
    _ <- takeBytes zeroBytes
    pure <<< update proxyBytesN <<< bytesDecode <<< unHex $ raw

instance abiEncodeVector :: (ABIEncode a, KnownSize n) => ABIEncode (Vector n a) where
  toDataBuilder as = foldMap toDataBuilder as

instance abiDecodeVector :: (ABIDecode a, KnownSize n) => ABIDecode (Vector n a) where
  fromDataParser =
    let
      len = sizeVal (Proxy :: Proxy n)
    in
      replicateA len fromDataParser

instance abiEncodeArray :: ABIEncode a => ABIEncode (Array a) where
  toDataBuilder as = uInt256HexBuilder (embed $ A.length as) <> foldMap toDataBuilder as

instance abiDecodeArray :: ABIDecode a => ABIDecode (Array a) where
  fromDataParser = do
    len <- unsafeToInt <$> uInt256HexParser
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

-- TODO(srghma): why ethereum developers decided to padRight?
-- https://ethereum.stackexchange.com/questions/121971/why-bytesn-elements-padded-to-the-right-but-all-other-elements-e-g-uintn ?
-- | base16 encode, then utf8 encode, then pad
bytesBuilder :: ByteString -> HexString
bytesBuilder = padRight <<< unsafePartial fromJust <<< mkHexString <<< flip BS.toString BS.Hex

-- | unsafe utfDecode
bytesDecode :: String -> ByteString
bytesDecode s = unsafePartial $ fromJust $ flip BS.fromString BS.Hex s

-- | Encode something that is essentaially a signed integer.

-- int256HexBuilder 1  == 0000000000000000000000000000000000000000000000000000000000000001
-- int256HexBuilder -1 == ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
-- int256HexBuilder -2 == fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe
int256HexBuilder :: BigNumber -> HexString
int256HexBuilder x =
  if x < zero then
    toHexString <<< negativeBigNumberToTwosComplement $ x -- not using `int256HexBuild` instead of `toHexString`, b.c. `toTwosComplement` doesn't return BigNumber with `-` prepended
  else
    padLeftSigned <<< toSignedHexString $ x

-- | Encode something that is essentially an unsigned integer.
uInt256HexBuilder :: BigNumber -> HexString
uInt256HexBuilder x =
  let
    Signed _ x' = toSignedHexString x
  in
    padLeft x'

-- | Parse as a signed `BigNumber`
int256HexParser :: forall m. Monad m => ParserT HexString m BigNumber
int256HexParser = toBigNumberFromSignedHexString <$> takeBytes 32

-- | Parse an unsigned `BigNumber`
uInt256HexParser :: forall m. Monad m => ParserT HexString m BigNumber
uInt256HexParser = toBigNumber <$> takeBytes 32

-- | Decode a `Boolean` as a BigNumber
fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

-- | Encode a `Boolean` as a `BigNumber`
toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

-- | Read any number of HexDigits
takeBytes :: forall m. Monad m => Int -> ParserT HexString m HexString
takeBytes n = A.fold <$> replicateA n takeByte

takeByte :: forall m. Monad m => ParserT HexString m HexString
takeByte = do
  ParseState input (Position position) _ <- get
  if hexLength input < 2 then
    fail "Unexpected EOF"
  else do
    let
      { after, before } = splitAt 2 (unHex input)

      unsafeMkHex s = unsafePartial $ fromJust $ mkHexString s

      position' = Position $ position { column = position.column + 1 }
    put $ ParseState (unsafeMkHex after) position' true
    pure $ unsafeMkHex before
