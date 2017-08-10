module Network.Ethereum.Web3.Encoding.Bytes where

import Prelude
import Data.ByteString (ByteString)
import Data.ByteString as BS
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (HexString(..), padRight)
import Node.Encoding (Encoding(Hex))
import Network.Ethereum.Web3.Encoding.Internal (class EncodingType)
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

bytesBuilder :: ByteString -> HexString
bytesBuilder = padRight <<< HexString <<< flip BS.toString Hex

bytesDecode :: String -> ByteString
bytesDecode = flip BS.fromString Hex

fromByteString :: forall n . KnownSize n => ByteString -> Maybe (BytesN n)
fromByteString bs = if BS.length bs > sizeVal (Proxy :: Proxy n)
                       then Nothing
                       else Just $ BytesN bs

--------------------------------------------------------------------------------
-- * Dynamic length byte array
--------------------------------------------------------------------------------

newtype BytesD = BytesD ByteString

unBytesD :: BytesD -> ByteString
unBytesD (BytesD bs) = bs

derive newtype instance eqBytesD :: Eq BytesD

derive newtype instance semigroupBytesD :: Semigroup BytesD

derive newtype instance monoidBytesD :: Monoid BytesD

instance showBytesD :: Show BytesD where
  show (BytesD bs) = BS.toString bs Hex

instance encodingTypeBytesD :: EncodingType BytesD where
  typeName  = const "bytes[]"
  isDynamic = const true

