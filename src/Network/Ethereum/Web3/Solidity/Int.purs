module Network.Ethereum.Web3.Solidity.Int
  ( IntN,
    unIntN,
    intFromBigNumber
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

instance showIntN :: KnownSize n => Show (IntN n) where
    show (IntN a) = show a

unIntN :: forall n . KnownSize n => IntN n -> BigNumber
unIntN (IntN a) = a

intFromBigNumber :: forall n . KnownSize n => BigNumber -> Maybe (IntN n)
intFromBigNumber a
  | a < zero = let minVal = negate $ (embed 2) `pow` (sizeVal (Proxy :: Proxy n) - 1)
            in if a < minVal then Nothing else Just <<< IntN $ a
  | otherwise = let maxVal = (embed 2) `pow` (sizeVal (Proxy :: Proxy n) - 1) -< 1
                in if a > maxVal then Nothing else Just <<< IntN $ a
