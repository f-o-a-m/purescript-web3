module Network.Ethereum.Web3.Solidity.UInt
  ( UIntN
  , unUIntN
  , uIntNFromBigNumber
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Network.Ethereum.Core.BigNumber (BigNumber, embed, pow)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- * Statically sized unsigned integers
--------------------------------------------------------------------------------
-- | Represents a statically sized unsigned integer of size `n`.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype UIntN (n :: Int) = UIntN BigNumber

derive newtype instance showUIntN :: Show (UIntN n)
derive newtype instance eqUIntN :: Eq (UIntN n)
derive newtype instance ordUIntN :: Ord (UIntN n)

-- | Access the raw underlying unsigned integer
unUIntN :: forall n. KnownSize n => UIntN n -> BigNumber
unUIntN (UIntN a) = a

-- | Attempt to coerce an unsigned integer into a statically sized one.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
uIntNFromBigNumber :: forall n. KnownSize n => Proxy n -> BigNumber -> Maybe (UIntN n)
uIntNFromBigNumber _ a
  | a < zero = Nothing
  | otherwise =
      let
        maxVal = (embed 2) `pow` (sizeVal (Proxy :: Proxy n)) - one
      in
        if a > maxVal then Nothing else Just <<< UIntN $ a
