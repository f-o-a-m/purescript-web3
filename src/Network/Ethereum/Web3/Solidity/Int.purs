module Network.Ethereum.Web3.Solidity.Int
  ( IntN,
    unIntN,
    intNFromBigNumber
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (BigNumber, embed, pow, (-<))
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized signed integers
--------------------------------------------------------------------------------

newtype IntN n = IntN BigNumber

derive newtype instance showIntN :: Show (IntN n)

derive newtype instance eqIntN :: Eq (IntN n)

unIntN :: forall n . KnownSize n => IntN n -> BigNumber
unIntN (IntN a) = a

intNFromBigNumber :: forall n . KnownSize n => BigNumber -> Maybe (IntN n)
intNFromBigNumber a
  | a < zero = let minVal = negate $ (embed 2) `pow` (sizeVal (Proxy :: Proxy n) - 1)
            in if a < minVal then Nothing else Just <<< IntN $ a
  | otherwise = let maxVal = (embed 2) `pow` (sizeVal (Proxy :: Proxy n) - 1) -< 1
                in if a > maxVal then Nothing else Just <<< IntN $ a
