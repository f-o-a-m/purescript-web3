module Network.Ethereum.Web3.Solidity.Vector
  ( Vector
  , unVector
  , nilVector
  , vCons
  , (:<)
  , vectorLength
  , toVector
  , generator
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen)
import Data.Array ((:))
import Data.Array as A
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable)
import Data.Unfoldable (class Unfoldable, class Unfoldable1, replicateA)
import Prim.Int (class Add)
import Type.Proxy (Proxy(..))

-- | Represents a statically sized vector of length `n`.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
newtype Vector (n :: Int) a = Vector (Array a)

derive newtype instance Show a => Show (Vector n a)
derive newtype instance Eq a => Eq (Vector n a)
derive newtype instance Functor (Vector n)
derive newtype instance Unfoldable1 (Vector n)
derive newtype instance Unfoldable (Vector n)
derive newtype instance Foldable (Vector n)
derive newtype instance Traversable (Vector n)

generator
  :: forall n m proxy a
   . Reflectable n Int
  => MonadGen m
  => proxy n
  -> m a
  -> m (Vector n a)
generator _ gen = Vector <$> replicateA (reflectType (Proxy @n)) gen

-- | Access the underlying array
unVector :: forall a n. Vector n a -> Array a
unVector (Vector as) = as

-- | Array of length 0
nilVector :: forall a. Vector 0 a
nilVector = Vector mempty

-- | Dependently typed `cons`
vCons :: forall a n nInc. Add n 1 nInc => a -> Vector n a -> Vector nInc a
vCons a (Vector as) = Vector (a : as)

infixr 6 vCons as :<

-- | Get the length of a statically sized vector
vectorLength :: forall a n. Vector n a -> Int
vectorLength (Vector as) = A.length as

-- | Attempt to coerce an array into a statically sized array.
-- | See module [Network.Ethereum.Web3.Solidity.Sizes](/Network.Ethereum.Web3.Solidity.Sizes) for some predefined sizes.
toVector :: forall a (n :: Int) proxy. Reflectable n Int => proxy n -> Array a -> Maybe (Vector n a)
toVector _ as =
  if reflectType (Proxy @n) /= A.length as then
    Nothing
  else
    Just (Vector as)
