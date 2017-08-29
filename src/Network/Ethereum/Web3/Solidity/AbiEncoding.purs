module Network.Ethereum.Web3.Solidity.AbiEncoding where

import Prelude
import Data.Maybe (Maybe, maybe)
import Control.Error.Util (hush)
import Data.Unfoldable (replicateA)
import Type.Proxy (Proxy(..))
import Data.Array (length) as A
import Data.String (fromCharArray)
import Data.ByteString (ByteString)
import Data.ByteString (toUTF8, fromUTF8, toString, fromString, length, Encoding(Hex)) as BS
import Text.Parsing.Parser.Token (hexDigit)
import Text.Parsing.Parser (Parser, ParserT, runParser, fail)
import Data.Foldable (foldMap)

import Network.Ethereum.Web3.Solidity.Size (class KnownNat, class ByteSize, class UIntSize, class IntSize, class KnownSize, sizeVal, natVal)
import Network.Ethereum.Web3.Solidity.Vector (Vector)
import Network.Ethereum.Web3.Solidity.Int (IntN, unIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Solidity.UInt (UIntN, unUIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Bytes (BytesN, unBytesN, update, proxyBytesN)
import Network.Ethereum.Web3.Types (class Algebra, Address(..), BigNumber, HexString(..), Signed(..),
                                    embed, fromHexStringSigned, padLeft, padLeftSigned, fromHexString,
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
    toDataBuilder  = uInt256HexBuilder <<< fromBool
    fromDataParser = toBool <$> uInt256HexParser

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
    uInt256HexBuilder (BS.length bytes) <> bytesBuilder bytes

  fromDataParser = do
    len <- toInt <$> fromDataParser
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
      len <- toInt <$> uInt256HexParser
      replicateA len fromDataParser

instance abiEncodingUint :: (KnownSize n, UIntSize n) => ABIEncoding (UIntN n) where
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

-- | Encode a big unsigned int
uInt256HexBuilder :: forall a . Algebra a BigNumber => a -> HexString
uInt256HexBuilder x =
  let Signed _ x' = toSignedHexString <<< embed $ x
  in padLeft x'

-- | Parse a big int
int256HexParser :: forall m . Monad m => ParserT String m BigNumber
int256HexParser = fromHexStringSigned <$> take 64

-- | Parse a big uint
uInt256HexParser :: forall m . Monad m => ParserT String m BigNumber
uInt256HexParser = fromHexString <$> take 64

-- | Boolean
fromBool :: Boolean -> BigNumber
fromBool b = if b then one else zero

toBool :: BigNumber -> Boolean
toBool bn = not $ bn == zero

-- | Read any number of HexDigits
take :: forall m . Monad m => Int -> ParserT String m HexString
take n = HexString <<< fromCharArray <$> replicateA n hexDigit
