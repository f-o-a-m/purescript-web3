module Network.Ethereum.Web3.Encoding.Vector
  ( Vector,
    unVector,
    nilVector,
    vCons, (:<),
    vectorLength,
    toVector
  ) where

import Prelude
import Data.Array (length) as A
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Type.Proxy (Proxy(..))

import Network.Ethereum.Web3.Encoding.Size (class KnownNat, S, Z, natVal)
import Network.Ethereum.Web3.Types (HexString)

data Vector n a = Vector (Array a)

unVector :: forall a n . Vector n a -> Array a
unVector (Vector as) = as

nilVector :: forall a . Vector Z a
nilVector = Vector mempty

vCons :: forall a n . a -> Vector n a -> Vector (S n) a
vCons a (Vector as) = Vector (a : as)

infixr 6 vCons as :<

vectorLength :: forall a n . KnownNat n => Vector n a -> Int
vectorLength (Vector as) = A.length as

toVector :: forall a n . KnownNat n => Array a -> Maybe (Vector n a)
toVector as = if natVal (Proxy :: Proxy n) /= A.length as
                 then Nothing
                 else Just (Vector as)
