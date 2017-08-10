module Network.Ethereum.Web3.Encoding.Bytes where

import Prelude
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (HexString(..), getPadLength, unHex)
import Node.Encoding (Encoding(Hex))
import Network.Ethereum.Web3.Encoding.EncodingType (class EncodingType)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, bytesBuilder, bytesDecode, take)
import Network.Ethereum.Web3.Encoding.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized byte array
--------------------------------------------------------------------------------

newtype BytesN n = BytesN ByteString

derive newtype instance eqBytesN :: Eq (BytesN n)

update :: forall n . KnownSize n => BytesN n -> ByteString -> BytesN n
update _ = BytesN

instance encodingTypeBytes :: KnownSize n => EncodingType (BytesN n) where
    typeName  = let n = show (sizeVal (Proxy :: Proxy n))
                in const $ "bytes[" <> n <> "]"
    isDynamic = const false

instance showNat :: KnownSize n => Show (BytesN n) where
    show (BytesN bs) = show <<< HexString $ BS.toString bs Hex

fromByteString :: forall n . KnownSize n => ByteString -> Maybe (BytesN n)
fromByteString bs = if BS.length bs > sizeVal (Proxy :: Proxy n)
                       then Nothing
                       else Just $ BytesN bs

instance abiEncodingBytesN :: KnownSize n => ABIEncoding (BytesN n) where
  toDataBuilder (BytesN bs) = bytesBuilder bs
  fromDataParser = do
    let result = (BytesN BS.empty :: BytesN n)
        len = sizeVal (Proxy :: Proxy n)
        zeroBytes = getPadLength (len * 2)
    raw <- take $ len * 2
    _ <- take $ zeroBytes
    pure <<< update result <<< bytesDecode <<< unHex $ raw

