module Network.Ethereum.Web3.Solidity.Vector
  ( Vector
  , unVector
  , nilVector
  , vCons
  , (:<)
  , vectorLength
  , toVector
  ) where

import Prelude
import Data.Array ((:))
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable)
import Data.Unfoldable (class Unfoldable, class Unfoldable1)
import Network.Ethereum.Web3.Solidity.Size (class Inc, class KnownSize, D0, DLProxy(DLProxy), DOne, sizeVal, DigitList)

-- | Represents a statically sized vector of length `n`.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype Vector (n :: DigitList) a
  = Vector (Array a)

derive newtype instance showVector :: Show a => Show (Vector n a)

derive newtype instance eqVector :: Eq a => Eq (Vector n a)

derive newtype instance functorVector :: Functor (Vector n)

derive newtype instance unfoldable1Vector :: Unfoldable1 (Vector n)

derive newtype instance unfoldableVector :: Unfoldable (Vector n)

derive newtype instance foldableVector :: Foldable (Vector n)

derive newtype instance traversableVector :: Traversable (Vector n)

-- | Access the underlying array
unVector :: forall a n. Vector n a -> Array a
unVector (Vector as) = as

-- | Array of length 0
nilVector :: forall a. Vector (DOne D0) a
nilVector = Vector mempty

-- | Dependently typed `cons`
vCons :: forall a n nInc. Inc n nInc => a -> Vector n a -> Vector nInc a
vCons a (Vector as) = Vector (a : as)

infixr 6 vCons as :<

-- | Get the length of a statically sized vector
vectorLength :: forall a n. KnownSize n => Vector n a -> Int
vectorLength (Vector as) = A.length as

-- | Attempt to coerce an array into a statically sized array.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
toVector :: forall a n. KnownSize n => DLProxy n -> Array a -> Maybe (Vector n a)
toVector _ as =
  if sizeVal (DLProxy :: DLProxy n) /= A.length as then
    Nothing
  else
    Just (Vector as)
