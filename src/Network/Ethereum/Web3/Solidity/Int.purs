module Network.Ethereum.Web3.Solidity.Int
  ( IntN,
    unIntN,
    intFromBigNumber
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Types (BigNumber, HexString(..), Signed(..), toSignedHexString, hexLength)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)

--------------------------------------------------------------------------------
-- * Statically sized unsigned integers
--------------------------------------------------------------------------------

newtype IntN n = IntN BigNumber

instance showIntN :: KnownSize n => Show (IntN n) where
    show (IntN a) = show a

unIntN :: forall n . KnownSize n => IntN n -> BigNumber
unIntN (IntN a) = a

intFromBigNumber :: forall n . KnownSize n => BigNumber -> Maybe (IntN n)
intFromBigNumber a
    | a < zero = Nothing
    | otherwise = let Signed _ hx = toSignedHexString a
                      size = hexLength (maybePad hx) `div` 2
                  in if size <= sizeVal (Proxy :: Proxy n) then Just <<< IntN $ a else Nothing
  where
    maybePad s = if hexLength s `mod` 2 == 0 then s else HexString "0" <> s
