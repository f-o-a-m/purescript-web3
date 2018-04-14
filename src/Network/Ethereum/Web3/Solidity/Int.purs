module Network.Ethereum.Web3.Solidity.Int
  ( IntN,
    unIntN,
    intNFromBigNumber
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3.Types (BigNumber, embed, pow)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- * Statically sized signed integers
--------------------------------------------------------------------------------

-- | Represents a statically sized signed integer of size `n` bytes
newtype IntN n = IntN BigNumber

derive newtype instance showIntN :: Show (IntN n)

derive newtype instance eqIntN :: Eq (IntN n)

derive newtype instance ordIntN :: Ord (IntN n)

-- | Access the raw underlying integer
unIntN :: forall n . KnownSize n => IntN n -> BigNumber
unIntN (IntN a) = a

-- | Attempt to coerce an signed `BigNumber` into a statically sized one
intNFromBigNumber :: forall n . KnownSize n => BigNumber -> Maybe (IntN n)
intNFromBigNumber a
  | a < zero = let minVal = negate $ (embed 2) `pow` (sizeVal (Proxy :: Proxy n) - one)
            in if a < minVal then Nothing else Just <<< IntN $ a
  | otherwise = let maxVal = (embed 2) `pow` (sizeVal (Proxy :: Proxy n) - one) - one
                in if a > maxVal then Nothing else Just <<< IntN $ a
