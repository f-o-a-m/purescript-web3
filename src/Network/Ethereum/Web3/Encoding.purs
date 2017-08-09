module Network.Ethereum.Web3.Encoding where

import Prelude
import Data.Maybe (Maybe)
import Control.Error.Util (hush)
import Data.ByteString (toUTF8, fromUTF8, empty, length) as BS
import Type.Proxy (Proxy(..))
import Text.Parsing.Parser (Parser, runParser)


import Network.Ethereum.Web3.Types
import Network.Ethereum.Web3.Encoding.Internal (int256HexBuilder, int256HexParser, take)
import Network.Ethereum.Web3.Encoding.Bytes (class BytesSize, bytesLength, BytesN(..), BytesD(..)
                                            , bytesBuilder, bytesDecode, unBytesD, update)

class ABIEncoding a where
  toDataBuilder :: a -> HexString
  fromDataParser :: Parser String a

instance abiEncodingAlgebra :: ABIEncoding BigNumber where
  toDataBuilder = int256HexBuilder
  fromDataParser = int256HexParser

-- | Parse encoded value, droping the leading '0x'
fromData :: forall a . ABIEncoding a => HexString -> Maybe a
fromData = hush <<< flip runParser fromDataParser <<< unHex


fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

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

instance abiEncodingBytesN :: BytesSize n => ABIEncoding (BytesN n) where
  toDataBuilder (BytesN bs) = bytesBuilder bs
  fromDataParser = do
    let result = (BytesN BS.empty :: BytesN n)
        len = bytesLength (Proxy :: Proxy n)
        zeroBytes = getPadLength (len * 2)
    raw <- take $ len * 2
    _ <- take $ zeroBytes
    pure <<< update result <<< bytesDecode <<< unHex $ raw

instance abiEncodingBytesD :: ABIEncoding BytesD where
  toDataBuilder (BytesD bytes) =
    int256HexBuilder (BS.length bytes) <> bytesBuilder bytes

  fromDataParser = do
    len <- toInt <$> int256HexParser
    BytesD <<< bytesDecode <<< unHex <$> take (len * 2)

instance abiEncodingString :: ABIEncoding String where
    toDataBuilder = toDataBuilder <<<  BytesD <<< BS.toUTF8
    fromDataParser = BS.fromUTF8 <<< unBytesD <$> fromDataParser

