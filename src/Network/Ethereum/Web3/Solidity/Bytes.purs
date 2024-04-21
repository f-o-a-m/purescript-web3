module Network.Ethereum.Web3.Solidity.Bytes
  ( BytesN
  , unBytesN
  , proxyBytesN
  , update
  , fromBuffer
  , generator
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as B
import Node.Encoding (Encoding(Hex))
import Data.Maybe (Maybe(..), fromJust)
import Data.Reflectable (class Reflectable, reflectType)
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Types (mkHexString)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- * Statically sized byte array
--------------------------------------------------------------------------------
-- Represents a statically sized bytestring of size `n` bytes.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype BytesN (n :: Int) = BytesN ImmutableBuffer

derive newtype instance eqBytesN :: Eq (BytesN n)
instance showBytesN :: Show (BytesN n) where
  show (BytesN bs) = show <<< unsafePartial fromJust <<< mkHexString $ B.toString Hex bs

generator :: forall n m. Reflectable n Int => MonadGen m => Proxy n -> m (BytesN n)
generator p = do
  bs <- Hex.generator (reflectType p)
  pure $ BytesN $ Hex.toBuffer bs

-- | Access the underlying raw bytestring
unBytesN :: forall n. BytesN n -> ImmutableBuffer
unBytesN (BytesN bs) = bs

proxyBytesN :: forall n. BytesN n
proxyBytesN = BytesN $ B.fromArray []

update :: forall n. BytesN n -> ImmutableBuffer -> BytesN n
update _ = BytesN

-- | Attempt to coerce a bytestring into one of the appropriate size.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
fromBuffer :: forall proxy n. Reflectable n Int => proxy n -> ImmutableBuffer -> Maybe (BytesN n)
fromBuffer _ bs =
  if not $ B.size bs <= reflectType (Proxy :: Proxy n) then
    Nothing
  else
    Just $ BytesN bs
