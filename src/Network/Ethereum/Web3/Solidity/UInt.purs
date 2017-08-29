module Network.Ethereum.Web3.Solidity.UInt
  ( UIntN,
    unUIntN,
    uIntNFromBigNumber
  ) where

import Prelude
import Data.String (length)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (BigNumber, embed, pow, (-<))
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized unsigned integers
--------------------------------------------------------------------------------

newtype UIntN n = UIntN BigNumber


derive newtype instance showUIntN :: Show (UIntN n)

derive newtype instance eqUIntN :: Eq (UIntN n)

unUIntN :: forall n . KnownSize n => UIntN n -> BigNumber
unUIntN (UIntN a) = a

uIntNFromBigNumber :: forall n . KnownSize n => BigNumber -> Maybe (UIntN n)
uIntNFromBigNumber a
  | a < zero = Nothing
  | otherwise = let maxVal = (embed 2) `pow` (sizeVal (Proxy :: Proxy n)) -< 1
                in if a > maxVal then Nothing else Just <<< UIntN $ a
