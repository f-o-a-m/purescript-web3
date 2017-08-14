module Network.Ethereum.Web3.Encoding.AbiEncoding where

import Prelude
import Data.Maybe (Maybe)
import Control.Error.Util (hush)
import Data.Unfoldable (replicateA)
import Type.Proxy (Proxy(..))
import Data.Array (length) as A
import Data.String (fromCharArray)
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, toString, fromString, length, Encoding(Hex)) as BS
import Text.Parsing.Parser.Token (hexDigit)
import Text.Parsing.Parser (Parser, ParserT, runParser)
import Data.Foldable (foldMap)

import Network.Ethereum.Web3.Encoding.Size (class KnownNat, class KnownSize, sizeVal, natVal)
import Network.Ethereum.Web3.Encoding.Vector (Vector)
import Network.Ethereum.Web3.Encoding.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Types (class Algebra, Address(..), BigNumber, HexString(..),
                                    embed, fromHexStringSigned, padLeft, padLeftSigned,
                                    getPadLength, padRight, toInt, toSignedHexString, toTwosComplement, unHex)

class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

instance abiEncodingAlgebra :: ABIEncoding BigNumber where
  toDataBuilder = int256HexBuilder
  fromDataParser = int256HexParser

-- | Parse encoded value, droping the leading '0x'
fromData :: forall a . ABIEncoding a => HexString -> Maybe a
fromData = hush <<< flip runParser fromDataParser <<< unHex

-- | Instances

instance abiEncodingBool :: ABIEncoding Boolean where
    toDataBuilder  = int256HexBuilder <<< fromBool
    fromDataParser = toBool <$> int256HexParser

instance abiEncodingInt :: ABIEncoding Int where
    toDataBuilder  = int256HexBuilder
    fromDataParser = toInt <$> int256HexParser

instance abiEncodingAddress :: ABIEncoding Address where
    toDataBuilder (Address addr) = padLeft addr
    fromDataParser = do
      _ <- take 24
      Address <$> take 40

instance abiEncodingBytesD :: ABIEncoding ByteString where
  toDataBuilder bytes =
    int256HexBuilder (BS.length bytes) <> bytesBuilder bytes

  fromDataParser = do
    len <- toInt <$> fromDataParser
    bytesDecode <<< unHex <$> take (len * 2)

instance abiEncodingString :: ABIEncoding String where
    toDataBuilder = toDataBuilder <<< BS.toUTF8
    fromDataParser = BS.fromUTF8 <$> fromDataParser

instance abiEncodingBytesN :: KnownSize n => ABIEncoding (BytesN n) where
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
    toDataBuilder as = toDataBuilder (A.length as) <> foldMap toDataBuilder as
    fromDataParser = do
      len <- toInt <$> fromDataParser
      replicateA len fromDataParser



--------------------------------------------------------------------------------
-- | Special Builders and Parsers
--------------------------------------------------------------------------------

-- | Bytestring
bytesBuilder :: ByteString -> HexString
bytesBuilder = padRight <<< HexString <<< flip BS.toString BS.Hex

bytesDecode :: String -> ByteString
bytesDecode = flip BS.fromString BS.Hex

-- | Encode anything any type of number that fits in a big numbed
int256HexBuilder :: forall a . Algebra a BigNumber => a -> HexString
int256HexBuilder x =
  let x' = embed x
  in if x' < zero
       then int256HexBuilder <<< toTwosComplement $ x'
       else padLeftSigned <<< toSignedHexString $ x'

-- | Parse a big number
int256HexParser :: forall m . Monad m => ParserT String m BigNumber
int256HexParser = fromHexStringSigned <$> take 64

-- | Boolean
fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

-- | Read any number of HexDigits
take :: forall m . Monad m => Int -> ParserT String m HexString
take n = HexString <<< fromCharArray <$> replicateA n hexDigit
