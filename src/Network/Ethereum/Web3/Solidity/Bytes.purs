module Network.Ethereum.Web3.Solidity.Bytes
  ( BytesN
  , unBytesN
  , proxyBytesN
  , update
  , fromByteString
  ) where

import Prelude
import Data.ByteString (ByteString)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, DLProxy(..), sizeVal, DigitList)

--------------------------------------------------------------------------------
-- * Statically sized byte array
--------------------------------------------------------------------------------
-- Represents a statically sized bytestring of size `n` bytes.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype BytesN (n :: DigitList)
  = BytesN Hex.HexString

derive newtype instance eqBytesN :: Eq (BytesN n)

derive newtype instance showBytesN :: Show (BytesN n)

derive newtype instance ordBytesN :: Ord (BytesN n)

-- | Access the underlying raw bytestring
unBytesN :: forall n. KnownSize n => BytesN n -> ByteString
unBytesN (BytesN bs) = Hex.toByteString bs

proxyBytesN :: forall n. KnownSize n => BytesN n
proxyBytesN = BytesN mempty

update :: forall n. KnownSize n => BytesN n -> ByteString -> BytesN n
update _ = BytesN <<< Hex.fromByteString

-- | Attempt to coerce a bytestring into one of the appropriate size.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
fromByteString :: forall n. KnownSize n => DLProxy n -> ByteString -> Maybe (BytesN n)
fromByteString _ bs =
  if not $ (Hex.hexLength h / 2) <= sizeVal (DLProxy :: DLProxy n) then
    Nothing
  else
    Just $ BytesN h
  where
  h = Hex.fromByteString bs
