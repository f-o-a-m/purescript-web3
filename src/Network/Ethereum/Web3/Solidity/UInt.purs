module Network.Ethereum.Web3.Solidity.UInt
  ( UIntN,
    unUIntN,
    uIntNFromBigNumber
  ) where

import Prelude
import Data.String (length)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (BigNumber, toString, hexadecimal)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized unsigned integers
--------------------------------------------------------------------------------

newtype UIntN n = UIntN BigNumber

instance showUIntN :: KnownSize n => Show (UIntN n) where
    show (UIntN a) = show a

unUIntN :: forall n . KnownSize n => UIntN n -> BigNumber
unUIntN (UIntN a) = a

uIntNFromBigNumber :: forall n . KnownSize n => BigNumber -> Maybe (UIntN n)
uIntNFromBigNumber a
    | a < zero = Nothing
    | otherwise = let hx = maybePad <<< toString hexadecimal $ a
                      size = length hx `div` 2
                  in if size <= sizeVal (Proxy :: Proxy n) then Just <<< UIntN $ a else Nothing
  where
    maybePad s = if length s `mod` 2 == 0 then s else "0" <> s

