module Network.Ethereum.Web3.Encoding.Bytes
  (BytesN,
   unBytesN,
   proxyBytesN,
   update,
   fromByteString
  ) where

import Prelude
import Data.ByteString (empty, ByteString, Encoding(Hex))
import Data.ByteString as BS
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (HexString(..))
import Network.Ethereum.Web3.Encoding.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized byte array
--------------------------------------------------------------------------------

newtype BytesN n = BytesN ByteString

derive newtype instance eqBytesN :: Eq (BytesN n)

instance showNat :: KnownSize n => Show (BytesN n) where
    show (BytesN bs) = show <<< HexString $ BS.toString bs Hex

unBytesN :: forall n . KnownSize n => BytesN n -> ByteString
unBytesN (BytesN bs) = bs

proxyBytesN :: forall n . KnownSize n => BytesN n
proxyBytesN = BytesN empty

update :: forall n . KnownSize n => BytesN n -> ByteString -> BytesN n
update _ = BytesN

fromByteString :: forall n . KnownSize n => ByteString -> Maybe (BytesN n)
fromByteString bs = if BS.length bs > sizeVal (Proxy :: Proxy n)
                       then Nothing
                       else Just $ BytesN bs

