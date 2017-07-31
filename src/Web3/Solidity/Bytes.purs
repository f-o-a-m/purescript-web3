module Web3.Solidity.Bytes where

import Prelude
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Type.Proxy (Proxy(..))


import Web3.Utils.Types (HexString(..), unHex)
import Node.Encoding (Encoding(Hex))
import Web3.Utils.Utils (padLeft, getPadLength)
import Web3.Solidity.Param (class EncodingType, take)
import Web3.Solidity.Encoding (class ABIEncoding)

data BytesN n = BytesN ByteString

update :: forall n . BytesSize n => BytesN n -> ByteString -> BytesN n
update _ = BytesN

instance encodingTypeBytes :: BytesSize n => EncodingType (BytesN n) where
    typeName  = const "bytes[N]"
    isDynamic = const false

instance showNat :: BytesSize n => Show (BytesN n) where
    show (BytesN bs) = show <<< HexString $ BS.toString bs Hex

bytesBuilder :: ByteString -> HexString
bytesBuilder = padLeft <<< HexString <<< flip BS.toString Hex

bytesDecode :: String -> ByteString
bytesDecode = flip BS.fromString Hex

instance abiEncodingBytesN :: BytesSize n => ABIEncoding (BytesN n) where
  toDataBuilder (BytesN bs) = bytesBuilder bs
  fromDataParser = do
    let result = (BytesN BS.empty :: BytesN n)
        len = bytesLength (Proxy :: Proxy n)
        zeroBytes = getPadLength len
    void <<< take $ zeroBytes * 2
    raw <- take $ len * 2
    pure <<< update result <<< bytesDecode <<< unHex $ raw


data B0
data B1
data B2
data B3
data B4
data B5
data B6
data B7
data B8
data B9

data NumCons a b
infix 6 type NumCons as :&

class BytesSize n where
  bytesLength :: Proxy n -> Int

instance bytesSizeB0 :: BytesSize B0 where
  bytesLength _ = 0

instance bytesSizeB1 :: BytesSize B1 where
  bytesLength _ = 1

instance bytesSizeB2 :: BytesSize B2 where
  bytesLength _ = 2

instance bytesSizeB3 :: BytesSize B3 where
  bytesLength _ = 3

instance bytesSizeB4 :: BytesSize B4 where
  bytesLength _ = 4

instance bytesSizeB5 :: BytesSize B5 where
  bytesLength _ = 5

instance bytesSizeB6 :: BytesSize B6 where
  bytesLength _ = 6

instance bytesSizeB7 :: BytesSize B7 where
  bytesLength _ = 7

instance bytesSizeB8 :: BytesSize B8 where
  bytesLength _ = 8

instance bytesSizeB9 :: BytesSize B9 where
  bytesLength _ = 9

instance bytesSizeCons :: (BytesSize tens, BytesSize ones) => BytesSize (tens :& ones) where
  bytesLength _ = 10 * (bytesLength (Proxy :: Proxy tens)) + bytesLength (Proxy :: Proxy ones)
