module Network.Ethereum.Web3.Solidity.Int
  ( IntN
  , unIntN
  , intNFromBigNumber
  , generator
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Network.Ethereum.Core.BigNumber (BigNumber, fromInt, fromString, fromTwosComplement, pow)
import Network.Ethereum.Core.HexString as Hex
import Partial.Unsafe (unsafeCrashWith)
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

generator :: forall n m. Reflectable n Int => MonadGen m => Proxy n -> m (IntN n)
generator p = do
  bs <- Hex.generator (reflectType p `div` 8)
  let
    n = reflectType (Proxy @n)
    a =
      if bs == mempty then zero
      else case fromString $ Hex.unHex $ bs of
        Nothing -> unsafeCrashWith $ "int" <> show n <> " generator: invalid hex string: " <> show bs
        Just x -> x
  pure $ IntN $ fromTwosComplement n a

-- | Access the raw underlying integer
unIntN :: forall n. IntN n -> BigNumber
unIntN (IntN a) = a

-- | Attempt to coerce an signed `BigNumber` into a statically sized one.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
intNFromBigNumber :: forall n proxy. Reflectable n Int => proxy n -> BigNumber -> Maybe (IntN n)
intNFromBigNumber _ a
  | a < zero =
      let
        minVal = negate $ (fromInt 2) `pow` (reflectType (Proxy @n) - one)
      in
        if a < minVal then Nothing else Just <<< IntN $ a
  | otherwise =
      let
        maxVal = (fromInt 2) `pow` (reflectType (Proxy @n) - one) - one
      in
        if a > maxVal then Nothing else Just <<< IntN $ a
