module Network.Ethereum.Web3.Solidity.Int
  ( IntN
  , unIntN
  , intNFromBigNumber
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Network.Ethereum.Core.BigNumber (BigNumber, embed, pow)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized signed integers
--------------------------------------------------------------------------------
-- | Represents a statically sized signed integer of size `n` bytes.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype IntN (n :: Int) = IntN BigNumber

derive newtype instance showIntN :: Show (IntN n)
derive newtype instance eqIntN :: Eq (IntN n)
derive newtype instance ordIntN :: Ord (IntN n)

-- | Access the raw underlying integer
unIntN :: forall n. IntN n -> BigNumber
unIntN (IntN a) = a

-- | Attempt to coerce an signed `BigNumber` into a statically sized one.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
intNFromBigNumber :: forall n proxy. KnownSize n => proxy n -> BigNumber -> Maybe (IntN n)
intNFromBigNumber proxy a
  | a < zero =
      let
        minVal = negate $ (embed 2) `pow` (sizeVal proxy - one)
      in
        if a < minVal then Nothing else Just <<< IntN $ a
  | otherwise =
      let
        maxVal = (embed 2) `pow` (sizeVal proxy - one) - one
      in
        if a > maxVal then Nothing else Just <<< IntN $ a
