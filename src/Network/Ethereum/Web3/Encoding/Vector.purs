module Network.Ethereum.Web3.Encoding.Vector
  ( Vector,
    nilVector,
    vCons, (:<),
    vectorLength,
    toVector,
    encodeArray
  ) where

import Prelude
import Data.Array (uncons, length) as A
import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Type.Proxy (Proxy(..))
import Data.Foldable (fold, foldMap)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Lens.Index (ix)
import Data.Lens.Setter (over)
import Text.Parsing.Parser (fail)

import Network.Ethereum.Web3.Encoding.Size (class KnownNat, S, Z, natVal)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder)
import Network.Ethereum.Web3.Types (HexString, hexLength)

data Vector n a = Vector (Array a)

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

accumulateOffsets :: Array Int -> Array Int
accumulateOffsets as = go 0 as
  where
    go :: Int -> Array Int -> Array Int
    go accum bs = case A.uncons bs of
      Nothing -> []
      Just ht -> let newAccum = accum + ht.head
                 in newAccum : go newAccum ht.tail

encodeArray :: forall a .
               ABIEncoding a
            => Array a
            -> HexString
encodeArray as =
    let countEncs = map countEnc as
        offsets = accumulateOffsets <<< over (ix 0) (\x -> x - 32) <<< map fst $ countEncs
        encodings = map snd countEncs
    in  foldMap toDataBuilder offsets <> fold encodings
  where
    countEnc a = let enc = toDataBuilder a
                 in Tuple (hexLength enc `div` 2) enc

instance abiEncodingArray :: (ABIEncoding a, KnownNat n) => ABIEncoding (Vector n a) where
    toDataBuilder (Vector as) = encodeArray as
    fromDataParser = fail "oops"
