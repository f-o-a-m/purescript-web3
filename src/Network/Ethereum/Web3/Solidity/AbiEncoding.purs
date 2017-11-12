module Network.Ethereum.Web3.Solidity.AbiEncoding where

import Prelude

import Control.Error.Util (hush)
import Data.Array (length) as A
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, toString, fromString, length, Encoding(Hex)) as BS
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, maybe, fromJust)
import Data.String (fromCharArray)
import Data.Unfoldable (replicateA)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Solidity.Int (IntN, unIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Size (class KnownNat, class ByteSize, class IntSize, sizeVal, natVal)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Vector (Vector)
import Network.Ethereum.Web3.Types (class Algebra, Address, BigNumber, HexString, Signed(..), embed, fromHexString, fromHexStringSigned, getPadLength, mkAddress, mkHexString, padLeft, padLeftSigned, padRight, toSignedHexString, toTwosComplement, unAddress, unHex, unsafeToInt)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser, ParserT, runParser, fail)
import Text.Parsing.Parser.Token (hexDigit)
import Type.Proxy (Proxy(..))


-- | Class representing values that have an encoding and decoding instance to/from a solidity type.
class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

instance abiEncodingAlgebra :: ABIEncoding BigNumber where
  toDataBuilder = int256HexBuilder
  fromDataParser = int256HexParser

-- | Parse encoded value, droping the leading `0x`
fromData :: forall a . ABIEncoding a => HexString -> Maybe a
fromData = hush <<< flip runParser fromDataParser <<< unHex

instance abiEncodingBool :: ABIEncoding Boolean where
    toDataBuilder  = uInt256HexBuilder <<< fromBool
    fromDataParser = toBool <$> uInt256HexParser

instance abiEncodingInt :: ABIEncoding Int where
    toDataBuilder  = int256HexBuilder
    fromDataParser = unsafeToInt <$> int256HexParser

instance abiEncodingAddress :: ABIEncoding Address where
    toDataBuilder addr = padLeft <<< unAddress $ addr
    fromDataParser = do
      _ <- take 24
      maddr <- mkAddress <$> take 40
      maybe (fail "Address is 20 bytes, receieved more") pure maddr

instance abiEncodingBytesD :: ABIEncoding ByteString where
  toDataBuilder bytes =
    uInt256HexBuilder (BS.length bytes) <> bytesBuilder bytes

  fromDataParser = do
    len <- unsafeToInt <$> fromDataParser
    bytesDecode <<< unHex <$> take (len * 2)

instance abiEncodingString :: ABIEncoding String where
    toDataBuilder = toDataBuilder <<< BS.toUTF8
    fromDataParser = BS.fromUTF8 <$> fromDataParser

instance abiEncodingBytesN :: ByteSize n => ABIEncoding (BytesN n) where
  toDataBuilder bs = bytesBuilder <<< unBytesN $ bs
  fromDataParser = do
    let len = sizeVal (Proxy :: Proxy n)
        zeroBytes = getPadLength (len * 2)
    raw <- take $ len * 2
    _ <- take $ zeroBytes
    pure <<< update proxyBytesN <<< bytesDecode <<< unHex $ raw

instance abiEncodingVector :: (ABIEncoding a, KnownNat n) => ABIEncoding (Vector n a) where
    toDataBuilder as = foldMap toDataBuilder as
    fromDataParser = let len = natVal (Proxy :: Proxy n)
                     in replicateA len fromDataParser

instance abiEncodingArray :: ABIEncoding a => ABIEncoding (Array a) where
    toDataBuilder as = uInt256HexBuilder (A.length as) <> foldMap toDataBuilder as
    fromDataParser = do
      len <- unsafeToInt <$> uInt256HexParser
      replicateA len fromDataParser

instance abiEncodingUint :: IntSize n => ABIEncoding (UIntN n) where
  toDataBuilder a = uInt256HexBuilder <<< unUIntN $ a
  fromDataParser = do
    a <- uInt256HexParser
    maybe (fail $ msg a) pure <<< uIntNFromBigNumber $ a
    where
      msg n = let size = sizeVal (Proxy :: Proxy n)
              in "Couldn't parse as uint" <> show size <> " : " <> show n

instance abiEncodingIntN :: IntSize n => ABIEncoding (IntN n) where
  toDataBuilder a = int256HexBuilder <<< unIntN $ a
  fromDataParser = do
    a <- int256HexParser
    maybe (fail $ msg a) pure <<< intNFromBigNumber $ a
    where
      msg n = let size = sizeVal (Proxy :: Proxy n)
              in "Couldn't parse as int" <> show size <> " : " <> show n

--------------------------------------------------------------------------------
-- | Special Builders and Parsers
--------------------------------------------------------------------------------

-- | base16 encode, then utf8 encode, then pad
bytesBuilder :: ByteString -> HexString
bytesBuilder = padRight <<< unsafePartial (fromJust <<< mkHexString) <<< flip BS.toString BS.Hex

-- | unsafe utfDecode
bytesDecode :: String -> ByteString
bytesDecode s = unsafePartial $ fromJust $ flip BS.fromString BS.Hex s

-- | Encode something that is essentaially a signed integer.
int256HexBuilder :: forall a . Algebra a BigNumber => a -> HexString
int256HexBuilder x =
  let x' = embed x
  in if x' < zero
       then int256HexBuilder <<< toTwosComplement $ x'
       else padLeftSigned <<< toSignedHexString $ x'

-- | Encode something that is essentially an unsigned integer.
uInt256HexBuilder :: forall a . Algebra a BigNumber => a -> HexString
uInt256HexBuilder x =
  let Signed _ x' = toSignedHexString <<< embed $ x
  in padLeft x'

-- | Parse as a signed `BigNumber`
int256HexParser :: forall m . Monad m => ParserT String m BigNumber
int256HexParser = fromHexStringSigned <$> take 64

-- | Parse an unsigned `BigNumber`
uInt256HexParser :: forall m . Monad m => ParserT String m BigNumber
uInt256HexParser = fromHexString <$> take 64

-- | Decode a `Boolean` as a BigNumber
fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

-- | Encode a `Boolean` as a `BigNumber`
toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

-- | Read any number of HexDigits
take :: forall m . Monad m => Int -> ParserT String m HexString
take n = unsafePartial (fromJust <<< mkHexString) <<< fromCharArray <$> replicateA n hexDigit
