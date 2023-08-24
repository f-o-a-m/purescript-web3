module Network.Ethereum.Web3.Solidity.Bytes
  ( BytesN
  , unBytesN
  , proxyBytesN
  , update
  , fromByteString
  ) where

import Prelude
import Data.ByteString (empty, ByteString, Encoding(Hex))
import Data.ByteString as BS
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, DLProxy(..), sizeVal, DigitList)
import Network.Ethereum.Types (mkHexString)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- * Statically sized byte array
--------------------------------------------------------------------------------
-- Represents a statically sized bytestring of size `n` bytes.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype BytesN (n :: DigitList)
  = BytesN ByteString

derive newtype instance eqBytesN :: Eq (BytesN n)

instance showBytesN :: KnownSize n => Show (BytesN n) where
  show (BytesN bs) = show <<< unsafePartial fromJust <<< mkHexString $ BS.toString bs Hex

-- | Access the underlying raw bytestring
unBytesN :: forall n. KnownSize n => BytesN n -> ByteString
unBytesN (BytesN bs) = bs

proxyBytesN :: forall n. KnownSize n => BytesN n
proxyBytesN = BytesN empty

update :: forall n. KnownSize n => BytesN n -> ByteString -> BytesN n
update _ = BytesN

-- | Attempt to coerce a bytestring into one of the appropriate size.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
fromByteString :: forall n. KnownSize n => DLProxy n -> ByteString -> Maybe (BytesN n)
fromByteString _ bs =
  if not $ BS.length bs <= sizeVal (DLProxy :: DLProxy n) then
    Nothing
  else
    Just $ BytesN bs
