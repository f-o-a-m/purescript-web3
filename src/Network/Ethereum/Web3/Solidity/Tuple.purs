module Network.Ethereum.Web3.Solidity.Tuple where

import Prelude

import Control.Monad.State.Class (get)
import Data.Array (reverse, (:))
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Tuple (Tuple(..), snd)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, toDataBuilder, fromDataParser, take)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Types (BigNumber, HexString, embed, hexLength, toInt)
import Text.Parsing.Parser (Parser, ParseState(..))
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))
import Type.Proxy (Proxy(..))


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
  toDataBuilder = uncurry1 $ _serialize (Tuple 1 [])
  fromDataParser = Singleton <$> factorParser

instance abiEncoding2 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b
                         ) => ABIEncoding (Tuple2 a b) where
  toDataBuilder = uncurry2 $ _serialize (Tuple 2 [])
  fromDataParser = Tuple2 <$> factorParser <*> factorParser

instance abiEncoding3 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c
                         ) => ABIEncoding (Tuple3 a b c) where
  toDataBuilder = uncurry3 $ _serialize (Tuple 3 [])
  fromDataParser = Tuple3 <$> factorParser <*> factorParser <*> factorParser

instance abiEncoding4 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d
                         ) => ABIEncoding (Tuple4 a b c d) where
  toDataBuilder = uncurry4 $ _serialize (Tuple 4 [])
  fromDataParser = Tuple4 <$> factorParser <*> factorParser <*> factorParser <*> factorParser

instance abiEncoding5 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d,
                          ABIEncoding e,
                          EncodingType e
                         ) => ABIEncoding (Tuple5 a b c d e) where
  toDataBuilder = uncurry5 $ _serialize (Tuple 5 [])
  fromDataParser = Tuple5 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser

instance abiEncoding6 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d,
                          ABIEncoding e,
                          EncodingType e,
                          ABIEncoding f,
                          EncodingType f
                         ) => ABIEncoding (Tuple6 a b c d e f) where
  toDataBuilder = uncurry6 $ _serialize (Tuple 6 [])
  fromDataParser = Tuple6 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser

instance abiEncoding7 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d,
                          ABIEncoding e,
                          EncodingType e,
                          ABIEncoding f,
                          EncodingType f,
                          ABIEncoding g,
                          EncodingType g
                         ) => ABIEncoding (Tuple7 a b c d e f g) where
  toDataBuilder = uncurry7 $ _serialize (Tuple 7 [])
  fromDataParser = Tuple7 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser

instance abiEncoding8 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d,
                          ABIEncoding e,
                          EncodingType e,
                          ABIEncoding f,
                          EncodingType f,
                          ABIEncoding g,
                          EncodingType g,
                          ABIEncoding h,
                          EncodingType h
                         ) => ABIEncoding (Tuple8 a b c d e f g h) where
  toDataBuilder = uncurry8 $ _serialize (Tuple 8 [])
  fromDataParser = Tuple8 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser

instance abiEncoding9 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d,
                          ABIEncoding e,
                          EncodingType e,
                          ABIEncoding f,
                          EncodingType f,
                          ABIEncoding g,
                          EncodingType g,
                          ABIEncoding h,
                          EncodingType h,
                          ABIEncoding i,
                          EncodingType i
                         ) => ABIEncoding (Tuple9 a b c d e f g h i) where
  toDataBuilder = uncurry9 $ _serialize (Tuple 9 [])
  fromDataParser = Tuple9 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                          <*> factorParser

instance abiEncoding10 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j
                          ) => ABIEncoding (Tuple10 a b c d e f g h i j) where
  toDataBuilder = uncurry10 $ _serialize (Tuple 10 [])
  fromDataParser = Tuple10 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser

instance abiEncoding11 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j,
                           ABIEncoding k,
                           EncodingType k
                          ) => ABIEncoding (Tuple11 a b c d e f g h i j k) where
  toDataBuilder = uncurry11 $ _serialize (Tuple 11 [])
  fromDataParser = Tuple11 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser <*> factorParser


instance abiEncoding12 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j,
                           ABIEncoding k,
                           EncodingType k,
                           ABIEncoding l,
                           EncodingType l
                          ) => ABIEncoding (Tuple12 a b c d e f g h i j k l) where
  toDataBuilder = uncurry12 $ _serialize (Tuple 12 [])
  fromDataParser = Tuple12 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser <*> factorParser <*> factorParser


instance abiEncoding13 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j,
                           ABIEncoding k,
                           EncodingType k,
                           ABIEncoding l,
                           EncodingType l,
                           ABIEncoding m,
                           EncodingType m
                          ) => ABIEncoding (Tuple13 a b c d e f g h i j k l m) where
  toDataBuilder = uncurry13 $ _serialize (Tuple 13 [])
  fromDataParser = Tuple13 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser


instance abiEncoding14 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j,
                           ABIEncoding k,
                           EncodingType k,
                           ABIEncoding l,
                           EncodingType l,
                           ABIEncoding m,
                           EncodingType m,
                           ABIEncoding n,
                           EncodingType n
                          ) => ABIEncoding (Tuple14 a b c d e f g h i j k l m n) where
  toDataBuilder = uncurry14 $ _serialize (Tuple 14 [])
  fromDataParser = Tuple14 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser


instance abiEncoding15 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j,
                           ABIEncoding k,
                           EncodingType k,
                           ABIEncoding l,
                           EncodingType l,
                           ABIEncoding m,
                           EncodingType m,
                           ABIEncoding n,
                           EncodingType n,
                           ABIEncoding o,
                           EncodingType o
                          ) => ABIEncoding (Tuple15 a b c d e f g h i j k l m n o) where
  toDataBuilder = uncurry15 $ _serialize (Tuple 15 [])
  fromDataParser = Tuple15 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser

instance abiEncoding16 :: (ABIEncoding a,
                           EncodingType a,
                           ABIEncoding b,
                           EncodingType b,
                           ABIEncoding c,
                           EncodingType c,
                           ABIEncoding d,
                           EncodingType d,
                           ABIEncoding e,
                           EncodingType e,
                           ABIEncoding f,
                           EncodingType f,
                           ABIEncoding g,
                           EncodingType g,
                           ABIEncoding h,
                           EncodingType h,
                           ABIEncoding i,
                           EncodingType i,
                           ABIEncoding j,
                           EncodingType j,
                           ABIEncoding k,
                           EncodingType k,
                           ABIEncoding l,
                           EncodingType l,
                           ABIEncoding m,
                           EncodingType m,
                           ABIEncoding n,
                           EncodingType n,
                           ABIEncoding o,
                           EncodingType o,
                           ABIEncoding p,
                           EncodingType p
                          ) => ABIEncoding (Tuple16 a b c d e f g h i j k l m n o p) where
  toDataBuilder = uncurry16 $ _serialize (Tuple 16 [])
  fromDataParser = Tuple16 <$> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser
                           <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser <*> factorParser

--------------------------------------------------------------------------------
newtype Singleton a = Singleton a

derive instance genericTuple1 :: Generic (Singleton a) _

unSingleton :: forall a . Singleton a -> a
unSingleton (Singleton a) = a

instance showSingleton :: Show a => Show (Singleton a) where
  show = genericShow

instance eqSingleton :: Eq a => Eq (Singleton a) where
  eq = genericEq

uncurry1 :: forall a b . (a -> b) -> Singleton a -> b
uncurry1 fun (Singleton a) = fun a

curry1 :: forall a b . (Singleton a -> b) -> a -> b
curry1 fun a = fun (Singleton a)


-- * Tuple2

data Tuple2 a b = Tuple2 a b

derive instance genericTuple2 :: Generic (Tuple2 a b) _

instance showTuple2 :: (Show a, Show b) => Show (Tuple2 a b) where
  show = genericShow

instance eqTuple2 :: (Eq a, Eq b) => Eq (Tuple2 a b) where
  eq = genericEq

uncurry2 :: forall a b c . (a -> b -> c) -> Tuple2 a b -> c
uncurry2 fun (Tuple2 a b) = fun a b

curry2 :: forall a b c . (Tuple2 a b -> c) -> a -> b -> c
curry2 fun a b = fun (Tuple2 a b)


-- * Tuple3

data Tuple3 a b c = Tuple3 a b c

derive instance genericTuple3 :: Generic (Tuple3 a b c) _

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show = genericShow

instance eqTuple3 :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c) where
  eq = genericEq

uncurry3 :: forall a b c d . (a -> b -> c -> d) -> Tuple3 a b c -> d
uncurry3 fun (Tuple3 a b c) = fun a b c

curry3 :: forall a b c d . (Tuple3 a b c -> d) -> a -> b -> c -> d
curry3 fun a b c = fun (Tuple3 a b c)

-- * Tuple4

data Tuple4 a b c d = Tuple4 a b c d

derive instance genericTuple4 :: Generic (Tuple4 a b c d) _

instance showTuple4 :: (Show a, Show b, Show c, Show d) => Show (Tuple4 a b c d) where
  show = genericShow

instance eqTuple4 :: (Eq a, Eq b, Eq c, Eq d) => Eq (Tuple4 a b c d) where
  eq = genericEq

uncurry4 :: forall a b c d e . (a -> b -> c -> d -> e) -> Tuple4 a b c d -> e
uncurry4 fun (Tuple4 a b c d) = fun a b c d

curry4 :: forall a b c d e. (Tuple4 a b c d-> e) -> a -> b -> c -> d -> e
curry4 fun a b c d = fun (Tuple4 a b c d)

-- * Tuple5

data Tuple5 a b c d e = Tuple5 a b c d e

derive instance genericTuple5 :: Generic (Tuple5 a b c d e) _

instance showTuple5 :: (Show a, Show b, Show c, Show d, Show e) => Show (Tuple5 a b c d e) where
  show = genericShow

instance eqTuple5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (Tuple5 a b c d e) where
  eq = genericEq

uncurry5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Tuple5 a b c d e -> f
uncurry5 fun (Tuple5 a b c d e) = fun a b c d e

curry5 :: forall a b c d e f. (Tuple5 a b c d e -> f) -> a -> b -> c -> d -> e -> f
curry5 fun a b c d e = fun (Tuple5 a b c d e)

-- * Tuple6

data Tuple6 a b c d e f = Tuple6 a b c d e f

derive instance genericTuple6 :: Generic (Tuple6 a b c d e f) _

instance showTuple6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => Show (Tuple6 a b c d e f) where
  show = genericShow

instance eqTuple6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (Tuple6 a b c d e f) where
  eq = genericEq

uncurry6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Tuple6 a b c d e f -> g
uncurry6 fun (Tuple6 a b c d e f) = fun a b c d e f

curry6 :: forall a b c d e f g. (Tuple6 a b c d e f -> g) -> a -> b -> c -> d -> e -> f -> g
curry6 fun a b c d e f = fun (Tuple6 a b c d e f)

-- * Tuple7

data Tuple7 a b c d e f g = Tuple7 a b c d e f g

derive instance genericTuple7 :: Generic (Tuple7 a b c d e f g) _

instance showTuple7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (Tuple7 a b c d e f g) where
  show = genericShow

instance eqTuple7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (Tuple7 a b c d e f g) where
  eq = genericEq

uncurry7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Tuple7 a b c d e f g -> h
uncurry7 fun (Tuple7 a b c d e f g) = fun a b c d e f g

curry7 :: forall a b c d e f g h. (Tuple7 a b c d e f g -> h) -> a -> b -> c -> d -> e -> f -> g -> h
curry7 fun a b c d e f g = fun (Tuple7 a b c d e f g)

-- * Tuple8

data Tuple8 a b c d e f g h = Tuple8 a b c d e f g h

derive instance genericTuple8 :: Generic (Tuple8 a b c d e f g h) _

instance showTuple8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (Tuple8 a b c d e f g h) where
  show = genericShow

instance eqTuple8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (Tuple8 a b c d e f g h) where
  eq = genericEq

uncurry8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Tuple8 a b c d e f g h -> i
uncurry8 fun (Tuple8 a b c d e f g h) = fun a b c d e f g h

curry8 :: forall a b c d e f g h i. (Tuple8 a b c d e f g h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> i
curry8 fun a b c d e f g h = fun (Tuple8 a b c d e f g h)

-- * Tuple9

data Tuple9 a b c d e f g h i = Tuple9 a b c d e f g h i

derive instance genericTuple9 :: Generic (Tuple9 a b c d e f g h i) _

instance showTuple9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (Tuple9 a b c d e f g h i) where
  show = genericShow

instance eqTuple9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (Tuple9 a b c d e f g h i) where
  eq = genericEq

uncurry9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Tuple9 a b c d e f g h i -> j
uncurry9 fun (Tuple9 a b c d e f g h i) = fun a b c d e f g h i

curry9 :: forall a b c d e f g h i j. (Tuple9 a b c d e f g h i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j
curry9 fun a b c d e f g h i = fun (Tuple9 a b c d e f g h i)

-- * Tuple10

data Tuple10 a b c d e f g h i j = Tuple10 a b c d e f g h i j

derive instance genericTuple10 :: Generic (Tuple10 a b c d e f g h i j) _

instance showTuple10 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (Tuple10 a b c d e f g h i j) where
  show = genericShow

instance eqTuple10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (Tuple10 a b c d e f g h i j) where
  eq = genericEq

uncurry10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Tuple10 a b c d e f g h i j -> k
uncurry10 fun (Tuple10 a b c d e f g h i j) = fun a b c d e f g h i j

curry10 :: forall a b c d e f g h i j k. (Tuple10 a b c d e f g h i j -> k) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
curry10 fun a b c d e f g h i j = fun (Tuple10 a b c d e f g h i j)


-- * Tuple11

data Tuple11 a b c d e f g h i j k = Tuple11 a b c d e f g h i j k

derive instance genericTuple11 :: Generic (Tuple11 a b c d e f g h i j k) _

instance showTuple11 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (Tuple11 a b c d e f g h i j k) where
  show = genericShow

instance eqTuple11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (Tuple11 a b c d e f g h i j k) where
  eq = genericEq

uncurry11 :: forall a b c d e f g h i j k l. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> Tuple11 a b c d e f g h i j k -> l
uncurry11 fun (Tuple11 a b c d e f g h i j k) = fun a b c d e f g h i j k

curry11 :: forall a b c d e f g h i j k l. (Tuple11 a b c d e f g h i j k -> l) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l
curry11 fun a b c d e f g h i j k = fun (Tuple11 a b c d e f g h i j k) 

-- * Tuple12

data Tuple12 a b c d e f g h i j k l = Tuple12 a b c d e f g h i j k l

derive instance genericTuple12 :: Generic (Tuple12 a b c d e f g h i j k l) _

instance showTuple12 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (Tuple12 a b c d e f g h i j k l) where
  show = genericShow

instance eqTuple12 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (Tuple12 a b c d e f g h i j k l) where
  eq = genericEq

uncurry12 :: forall a b c d e f g h i j k l m. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m) -> Tuple12 a b c d e f g h i j k l -> m
uncurry12 fun (Tuple12 a b c d e f g h i j k l) = fun a b c d e f g h i j k l

curry12 :: forall a b c d e f g h i j k l m. (Tuple12 a b c d e f g h i j k l -> m) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m
curry12 fun a b c d e f g h i j k l = fun (Tuple12 a b c d e f g h i j k l)


-- * Tuple13

data Tuple13 a b c d e f g h i j k l m = Tuple13 a b c d e f g h i j k l m

derive instance genericTuple13 :: Generic (Tuple13 a b c d e f g h i j k l m) _

instance showTuple13 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (Tuple13 a b c d e f g h i j k l m) where
  show = genericShow

instance eqTuple13 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (Tuple13 a b c d e f g h i j k l m) where
  eq = genericEq

uncurry13 :: forall a b c d e f g h i j k l m n. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n) -> Tuple13 a b c d e f g h i j k l m -> n
uncurry13 fun (Tuple13 a b c d e f g h i j k l m) = fun a b c d e f g h i j k l m

curry13 :: forall a b c d e f g h i j k l m n. (Tuple13 a b c d e f g h i j k l m -> n) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n
curry13 fun a b c d e f g h i j k l m = fun (Tuple13 a b c d e f g h i j k l m)


-- * Tuple14

data Tuple14 a b c d e f g h i j k l m n = Tuple14 a b c d e f g h i j k l m n

derive instance genericTuple14 :: Generic (Tuple14 a b c d e f g h i j k l m n) _

instance showTuple14 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (Tuple14 a b c d e f g h i j k l m n) where
  show = genericShow

instance eqTuple14 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (Tuple14 a b c d e f g h i j k l m n) where
  eq = genericEq

uncurry14 :: forall a b c d e f g h i j k l m n o. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o) -> Tuple14 a b c d e f g h i j k l m n -> o
uncurry14 fun (Tuple14 a b c d e f g h i j k l m n) = fun a b c d e f g h i j k l m n

curry14 :: forall a b c d e f g h i j k l m n o. (Tuple14 a b c d e f g h i j k l m n -> o) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o
curry14 fun a b c d e f g h i j k l m n = fun (Tuple14 a b c d e f g h i j k l m n)

-- * Tuple15

data Tuple15 a b c d e f g h i j k l m n o = Tuple15 a b c d e f g h i j k l m n o

derive instance genericTuple15 :: Generic (Tuple15 a b c d e f g h i j k l m n o) _

instance showTuple15 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (Tuple15 a b c d e f g h i j k l m n o) where
  show = genericShow

instance eqTuple15 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (Tuple15 a b c d e f g h i j k l m n o) where
  eq = genericEq

uncurry15 :: forall a b c d e f g h i j k l m n o p. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p) -> Tuple15 a b c d e f g h i j k l m n o -> p
uncurry15 fun (Tuple15 a b c d e f g h i j k l m n o) = fun a b c d e f g h i j k l m n o

curry15 :: forall a b c d e f g h i j k l m n o p. (Tuple15 a b c d e f g h i j k l m n o -> p) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p
curry15 fun a b c d e f g h i j k l m n o = fun (Tuple15 a b c d e f g h i j k l m n o)


-- * Tuple16

data Tuple16 a b c d e f g h i j k l m n o p = Tuple16 a b c d e f g h i j k l m n o p

derive instance genericTuple16 :: Generic (Tuple16 a b c d e f g h i j k l m n o p) _

instance showTuple16 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) => Show (Tuple16 a b c d e f g h i j k l m n o p) where
  show = genericShow

instance eqTuple16 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p) => Eq (Tuple16 a b c d e f g h i j k l m n o p) where
  eq = genericEq

uncurry16 :: forall a b c d e f g h i j k l m n o p q. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q) -> Tuple16 a b c d e f g h i j k l m n o p -> q
uncurry16 fun (Tuple16 a b c d e f g h i j k l m n o p) = fun a b c d e f g h i j k l m n o p

curry16 :: forall a b c d e f g h i j k l m n o p q. (Tuple16 a b c d e f g h i j k l m n o p -> q) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q
curry16 fun a b c d e f g h i j k l m n o p = fun (Tuple16 a b c d e f g h i j k l m n o p) 



--------------------------------------------------------------------------------

factorParser :: forall a . ABIEncoding a => EncodingType a => Parser String a
factorParser
  | not $ isDynamic (Proxy :: Proxy a) = fromDataParser
  | otherwise = dParser

dParser :: forall a . ABIEncoding a => Parser String a
dParser = do
  dataOffset <- toInt <$> fromDataParser
  lookAhead $ do
    (ParseState _ (Position p) _) <- get
    _ <- take (dataOffset * 2 - (p.column - 1))
    fromDataParser
