module Network.Ethereum.Web3.Solidity.Int
  ( IntN
  , unIntN
  , intNFromBigNumber
  ) where

import Prelude

import Control.Monad.Gen as Gen
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty(..))
import Network.Ethereum.Core.BigNumber (BigNumber, embed, fromString, pow)
import Network.Ethereum.Core.HexString (genBytes, unHex)
import Network.Ethereum.Web3.Solidity.Size (class KnownSize, sizeVal)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- * Statically sized signed integers
--------------------------------------------------------------------------------
-- | Represents a statically sized signed integer of size `n` bytes.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype IntN (n :: Int) = IntN BigNumber

derive newtype instance showIntN :: Show (IntN n)
derive newtype instance eqIntN :: Eq (IntN n)
derive newtype instance ordIntN :: Ord (IntN n)

instance KnownSize n => Arbitrary (IntN n) where
  arbitrary = do
    sign <- embed <$> Gen.elements (NonEmpty (-1) [ 1 ])
    nBytes <- (flip div 8) <$> Gen.chooseInt 1 (sizeVal (Proxy @n))
    ma <- fromString <<< unHex <$> genBytes nBytes
    let a = unsafePartial $ fromJust ma
    pure <<< IntN $ sign * a

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
