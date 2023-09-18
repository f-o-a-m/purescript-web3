module Network.Ethereum.Web3.Solidity.UInt
  ( UIntN
  , unUIntN
  , uIntNFromBigNumber
  , generator
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Maybe (Maybe(..), fromJust)
import Data.Reflectable (class Reflectable, reflectType)
import Network.Ethereum.Core.BigNumber (BigNumber, fromInt, fromString, pow)
import Network.Ethereum.Core.HexString as Hex
import Partial.Unsafe (unsafePartial)
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

generator
  :: forall n m
   . Reflectable n Int
  => MonadGen m
  => Proxy n
  -> m (UIntN n)
generator p = do
  nBytes <- (flip div 8) <$> chooseInt 1 (reflectType p)
  bs <- Hex.generator nBytes
  let
    a =
      if bs == mempty then zero
      else unsafePartial $ fromJust $ fromString $ Hex.unHex bs
  pure $ UIntN $ if a < zero then -a else a

-- | Access the raw underlying unsigned integer
unUIntN :: forall n. UIntN n -> BigNumber
unUIntN (UIntN a) = a

-- | Attempt to coerce an unsigned integer into a statically sized one.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
uIntNFromBigNumber :: forall n. Reflectable n Int => Proxy n -> BigNumber -> Maybe (UIntN n)
uIntNFromBigNumber _ a
  | a < zero = Nothing
  | otherwise =
      let
        maxVal = (fromInt 2) `pow` (reflectType (Proxy :: Proxy n)) - one
      in
        if a > maxVal then Nothing else Just <<< UIntN $ a
