module Network.Ethereum.Web3.Encoding.Tuple where

import Prelude
import Data.Array (reverse, (:))
import Data.Tuple (Tuple(..), snd)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Foldable (fold, foldMap)
import Type.Proxy (Proxy(..))
import Text.Parsing.Parser(fail)

import Network.Ethereum.Web3.Types (BigNumber, HexString, embed, hexLength)
import Network.Ethereum.Web3.Encoding.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder, fromDataParser, take)

-- | Argument offset calculator
offset :: Int
       -- ^ Count of arguments
       -> Array HexString
       -- ^ Previous dynamic arguments
       -> Int
       -- ^ Offset
offset totalArgs args = headerOffset + dataOffset
  where
    headerOffset = totalArgs * 32
    dataOffset   = let (Additive rawLength) = foldMap (Additive <<< hexLength) args
                   in rawLength `div` 2

data EncodedValue =
  EncodedValue { headEnc :: HexString
               , tailEnc :: HexString
               }

instance encodedValueSemigroup :: Semigroup EncodedValue where
  append (EncodedValue val1) (EncodedValue val2) =
    EncodedValue { headEnc : val1.headEnc <> val2.headEnc
                 , tailEnc : val1.tailEnc <> val2.tailEnc
                 }

instance encodedValueMonoid :: Monoid EncodedValue where
  mempty = EncodedValue {headEnc : mempty, tailEnc : mempty}

-- | ABI data multiparam internal serializer
class ABIData a where
    _serialize :: Tuple Int (Array EncodedValue) -> a
    -- ^ Serialize with accumulator:
    -- pair of argument count and list of pair header and
    -- data part (for dynamic arguments)

instance abiDataHexString :: ABIData HexString where
    _serialize = (\(EncodedValue e) -> e.headEnc <> e.tailEnc) <<< fold <<< reverse <<< snd

instance abiDataInductive :: (EncodingType b, ABIEncoding b, ABIData a) => ABIData (b -> a) where
  _serialize (Tuple n l) x =
    if isDynamic (Proxy :: Proxy b)
       then _serialize $ Tuple n (dynEncoding  : l)
       else _serialize $ Tuple n (staticEncoding  : l)
    where
      dynOffset = offset n <<< map (\(EncodedValue a) -> a.tailEnc) $ l
      dynEncoding = EncodedValue { headEnc : (toDataBuilder (embed dynOffset :: BigNumber))
                                 , tailEnc : toDataBuilder x
                                 }
      staticEncoding = EncodedValue { headEnc : toDataBuilder x
                                    , tailEnc : mempty
                                    }


instance abiEncoding1 :: (ABIEncoding a,
                          EncodingType a
                         ) => ABIEncoding (Singleton a) where
  toDataBuilder (Singleton a) = _serialize (Tuple 1 []) a
  fromDataParser = fail "oops"

instance abiEncoding2 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b
                         ) => ABIEncoding (Tuple2 a b) where
  toDataBuilder (Tuple2 a b) = _serialize (Tuple 2 []) a b
  fromDataParser = fail "oops"

instance abiEncoding3 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c
                         ) => ABIEncoding (Tuple3 a b c) where
  toDataBuilder (Tuple3 a b c) = _serialize (Tuple 3 []) a b c
  fromDataParser = fail "oops"

--------------------------------------------------------------------------------
data Singleton a = Singleton a

uncurry1 :: forall a b . (a -> b) -> Singleton a -> b
uncurry1 f (Singleton a) = f a

curry1 :: forall a b . (Singleton a -> b) -> a -> b
curry1 f a = f (Singleton a)

data Tuple2 a b = Tuple2 a b

uncurry2 :: forall a b c . (a -> b -> c) -> Tuple2 a b -> c
uncurry2 f (Tuple2 a b) = f a b

curry2 :: forall a b c . (Tuple2 a b -> c) -> a -> b -> c
curry2 f a b = f (Tuple2 a b)

data Tuple3 a b c = Tuple3 a b c

uncurry3 :: forall a b c d . (a -> b -> c -> d) -> Tuple3 a b c -> d
uncurry3 f (Tuple3 a b c) = f a b c

curry3 :: forall a b c d . (Tuple3 a b c -> d) -> a -> b -> c -> d
curry3 f a b c = f (Tuple3 a b c)

---- | Static argument parser
--sParser :: EncodingType a => ABIEncoding a => a -> Parser a
--sParser x | isDynamic x = take 64 >>= \_ -> pure undefined
--          | otherwise   = fromDataParser
--
---- | Dynamic argument parser
--dParser :: EncodingType a => ABIEncoding a => a -> Parser a
--dParser x | isDynamic x = fromDataParser
--          | otherwise   = pure x
