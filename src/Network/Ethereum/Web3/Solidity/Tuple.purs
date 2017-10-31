module Network.Ethereum.Web3.Solidity.Tuple where

import Prelude

import Control.Monad.State.Class (get)
import Data.Array (reverse, (:))
import Data.Foldable (fold, foldMap)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
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
  toDataBuilder (Singleton a) = _serialize (Tuple 1 []) a
  fromDataParser = factorParser >>= pure <<< Singleton

instance abiEncoding2 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b
                         ) => ABIEncoding (Tuple2 a b) where
  toDataBuilder (Tuple2 a b) = _serialize (Tuple 2 []) a b
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    pure $ Tuple2 a b

instance abiEncoding3 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c
                         ) => ABIEncoding (Tuple3 a b c) where
  toDataBuilder (Tuple3 a b c) = _serialize (Tuple 3 []) a b c
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    pure $ Tuple3 a b c

instance abiEncoding4 :: (ABIEncoding a,
                          EncodingType a,
                          ABIEncoding b,
                          EncodingType b,
                          ABIEncoding c,
                          EncodingType c,
                          ABIEncoding d,
                          EncodingType d
                         ) => ABIEncoding (Tuple4 a b c d) where
  toDataBuilder (Tuple4 a b c d) = _serialize (Tuple 4 []) a b c d
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    pure $ Tuple4 a b c d

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
  toDataBuilder (Tuple5 a b c d e) = _serialize (Tuple 5 []) a b c d e
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    pure $ Tuple5 a b c d e

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
  toDataBuilder (Tuple6 a b c d e f) = _serialize (Tuple 6 []) a b c d e f
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    pure $ Tuple6 a b c d e f

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
  toDataBuilder (Tuple7 a b c d e f g) = _serialize (Tuple 7 []) a b c d e f g
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    pure $ Tuple7 a b c d e f g

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
  toDataBuilder (Tuple8 a b c d e f g h) = _serialize (Tuple 8 []) a b c d e f g h
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    pure $ Tuple8 a b c d e f g h

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
  toDataBuilder (Tuple9 a b c d e f g h i) = _serialize (Tuple 9 []) a b c d e f g h i
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    pure $ Tuple9 a b c d e f g h i

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
  toDataBuilder (Tuple10 a b c d e f g h i j) = _serialize (Tuple 10 []) a b c d e f g h i j
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    pure $ Tuple10 a b c d e f g h i j

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
  toDataBuilder (Tuple11 a b c d e f g h i j k) = _serialize (Tuple 11 []) a b c d e f g h i j k
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    k <- factorParser
    pure $ Tuple11 a b c d e f g h i j k

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
                          ) => ABIEncoding (Tuple12 a b c d e f g h i j h k l) where
  toDataBuilder (Tuple12 a b c d e f g h i j k l) = _serialize (Tuple 12 []) a b c d e f g h i j k l
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    k <- factorParser
    l <- factorParser
    pure $ Tuple12 a b c d e f g h i j k l


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
                          ) => ABIEncoding (Tuple13 a b c d e f g h i j h k l m) where
  toDataBuilder (Tuple13 a b c d e f g h i j k l m) = _serialize (Tuple 13 []) a b c d e f g h i j k l m
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    k <- factorParser
    l <- factorParser
    m <- factorParser
    pure $ Tuple13 a b c d e f g h i j k l m


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
                          ) => ABIEncoding (Tuple14 a b c d e f g h i j h k l m n) where
  toDataBuilder (Tuple14 a b c d e f g h i j k l m n) = _serialize (Tuple 14 []) a b c d e f g h i j k l m n
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    k <- factorParser
    l <- factorParser
    m <- factorParser
    n <- factorParser
    pure $ Tuple14 a b c d e f g h i j k l m n


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
                          ) => ABIEncoding (Tuple15 a b c d e f g h i j h k l m n o) where
  toDataBuilder (Tuple15 a b c d e f g h i j k l m n o) = _serialize (Tuple 15 []) a b c d e f g h i j k l m n o
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    k <- factorParser
    l <- factorParser
    m <- factorParser
    n <- factorParser
    o <- factorParser
    pure $ Tuple15 a b c d e f g h i j k l m n o

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
                          ) => ABIEncoding (Tuple16 a b c d e f g h i j h k l m n o p) where
  toDataBuilder (Tuple16 a b c d e f g h i j k l m n o p) = _serialize (Tuple 16 []) a b c d e f g h i j k l m n o p
  fromDataParser = do
    a <- factorParser
    b <- factorParser
    c <- factorParser
    d <- factorParser
    e <- factorParser
    f <- factorParser
    g <- factorParser
    h <- factorParser
    i <- factorParser
    j <- factorParser
    k <- factorParser
    l <- factorParser
    m <- factorParser
    n <- factorParser
    o <- factorParser
    p <- factorParser
    pure $ Tuple16 a b c d e f g h i j k l m n o p

--------------------------------------------------------------------------------
newtype Singleton a = Singleton a

unSingleton :: forall a . Singleton a -> a
unSingleton (Singleton a) = a

instance showSingleton :: Show a => Show (Singleton a) where
  show (Singleton a) = makeTupleString [show a]

instance eqSingleton :: Eq a => Eq (Singleton a) where
  eq (Singleton a) (Singleton b) = a == b

uncurry1 :: forall a b . (a -> b) -> Singleton a -> b
uncurry1 fun (Singleton a) = fun a

curry1 :: forall a b . (Singleton a -> b) -> a -> b
curry1 fun a = fun (Singleton a)


-- * Tuple2

data Tuple2 a b = Tuple2 a b

instance showTuple2 :: (Show a, Show b) => Show (Tuple2 a b) where
  show (Tuple2 a b) = makeTupleString [ show a
                                      , show b
                                      ]

instance eqTuple2 :: (Eq a, Eq b) => Eq (Tuple2 a b) where
  eq (Tuple2 a b) (Tuple2 a' b') = a == a' && b == b'

-- * Tuple3

uncurry2 :: forall a b c . (a -> b -> c) -> Tuple2 a b -> c
uncurry2 fun (Tuple2 a b) = fun a b

curry2 :: forall a b c . (Tuple2 a b -> c) -> a -> b -> c
curry2 fun a b = fun (Tuple2 a b)

data Tuple3 a b c = Tuple3 a b c

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show (Tuple3 a b c) = makeTupleString [ show a
                                        , show b
                                        , show c
                                        ]

instance eqTuple3 :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c) where
  eq (Tuple3 a b c) (Tuple3 a' b' c') =
    a == a' && b == b' && c == c'

uncurry3 :: forall a b c d . (a -> b -> c -> d) -> Tuple3 a b c -> d
uncurry3 fun (Tuple3 a b c) = fun a b c

curry3 :: forall a b c d . (Tuple3 a b c -> d) -> a -> b -> c -> d
curry3 fun a b c = fun (Tuple3 a b c)

-- * Tuple4

data Tuple4 a b c d = Tuple4 a b c d

instance showTuple4 :: (Show a, Show b, Show c, Show d) => Show (Tuple4 a b c d) where
  show (Tuple4 a b c d) = makeTupleString [ show a
                                          , show b
                                          , show c
                                          , show d
                                          ]

instance eqTuple4 :: (Eq a, Eq b, Eq c, Eq d) => Eq (Tuple4 a b c d) where
  eq (Tuple4 a b c d) (Tuple4 a' b' c' d') =
    a == a' && b == b' && c == c' && d == d'

uncurry4 :: forall a b c d e . (a -> b -> c -> d -> e) -> Tuple4 a b c d -> e
uncurry4 fun (Tuple4 a b c d) = fun a b c d

curry4 :: forall a b c d e. (Tuple4 a b c d-> e) -> a -> b -> c -> d -> e
curry4 fun a b c d = fun (Tuple4 a b c d)

-- * Tuple5

data Tuple5 a b c d e = Tuple5 a b c d e

instance showTuple5 :: (Show a, Show b, Show c, Show d, Show e) => Show (Tuple5 a b c d e) where
  show (Tuple5 a b c d e) = makeTupleString [ show a
                                            , show b
                                            , show c
                                            , show d
                                            , show e
                                            ]

instance eqTuple5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (Tuple5 a b c d e) where
  eq (Tuple5 a b c d e) (Tuple5 a' b' c' d' e') =
    a == a' && b == b' && c == c' && d == d' && e == e'

uncurry5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Tuple5 a b c d e -> f
uncurry5 fun (Tuple5 a b c d e) = fun a b c d e

curry5 :: forall a b c d e f. (Tuple5 a b c d e -> f) -> a -> b -> c -> d -> e -> f
curry5 fun a b c d e = fun (Tuple5 a b c d e)

-- * Tuple6

data Tuple6 a b c d e f = Tuple6 a b c d e f

instance showTuple6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => Show (Tuple6 a b c d e f) where
  show (Tuple6 a b c d e f) = makeTupleString [ show a
                                              , show b
                                              , show c
                                              , show d
                                              , show e
                                              , show f
                                              ]

instance eqTuple6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (Tuple6 a b c d e f) where
  eq (Tuple6 a b c d e f) (Tuple6 a' b' c' d' e' f') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

uncurry6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Tuple6 a b c d e f -> g
uncurry6 fun (Tuple6 a b c d e f) = f a b c d e f' g

curry6 :: forall a b c d e f g. (Tuple6 a b c d e f -> g) -> a -> b -> c -> d -> e -> f -> g
curry6 fun a b c d e f = fun (Tuple6 a b c d e f)

-- * Tuple7

data Tuple7 a b c d e f g = Tuple7 a b c d e f g

instance showTuple7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (Tuple7 a b c d e f g) where
  show (Tuple7 a b c d e f g) = makeTupleString [ show a
                                                , show b
                                                , show c
                                                , show d
                                                , show e
                                                , show f
                                                , show g
                                                ]

instance eqTuple7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (Tuple7 a b c d e f g) where
  eq (Tuple7 a b c d e f g) (Tuple7 a' b' c' d' e' f' g') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g'

uncurry7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Tuple7 a b c d e f g -> h
uncurry7 fun (Tuple7 a b c d e f g) = fun a b c d e f g

curry7 :: forall a b c d e f g h. (Tuple7 a b c d e f g -> h) -> a -> b -> c -> d -> e -> f -> g -> h
curry7 fun a b c d e f g = fun (Tuple7 a b c d e f g)

-- * Tuple8

data Tuple8 a b c d e f g h = Tuple8 a b c d e f g h

instance showTuple8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (Tuple8 a b c d e f g h) where
  show (Tuple8 a b c d e f g h) = makeTupleString [ show a
                                                  , show b
                                                  , show c
                                                  , show d
                                                  , show e
                                                  , show f
                                                  , show g
                                                  , show h
                                                  ]

instance eqTuple8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (Tuple8 a b c d e f g h) where
  eq (Tuple8 a b c d e f g h) (Tuple8 a' b' c' d' e' f' g' h') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h'

uncurry8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Tuple8 a b c d e f g h -> i
uncurry8 fun (Tuple8 a b c d f g h) = fun a b c d e f g h

curry8 :: forall a b c d e f g h i. (Tuple8 a b c d e f g h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> i
curry8 fun a b c d e f g h i = fun (Tuple8 a b c d e f g)

-- * Tuple9

data Tuple9 a b c d e f g h i = Tuple9 a b c d e f g h i

instance showTuple9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (Tuple9 a b c d e f g h i) where
  show (Tuple9 a b c d e f g h i) = makeTupleString [ show a
                                                    , show b
                                                    , show c
                                                    , show d
                                                    , show e
                                                    , show f
                                                    , show g
                                                    , show h
                                                    , show i
                                                    ]

instance eqTuple9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (Tuple9 a b c d e f g h i) where
  eq (Tuple9 a b c d e f g h i) (Tuple9 a' b' c' d' e' f' g' h' i') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i'

uncurry9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Tuple9 a b c d e f g h i -> j
uncurry9 fun (Tuple9 a b c d e f g h i) = fun a b c d e f g h i

curry9 :: forall a b c d e f g h i j. (Tuple9 a b c d e f g h i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j
curry9 fun a b c d e f g h i = fun (Tuple9 a b c d e f g h i)

-- * Tuple10

data Tuple10 a b c d e f g h i j = Tuple10 a b c d e f g h i j

instance showTuple10 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (Tuple10 a b c d e f g h i j) where
  show (Tuple10 a b c d e f g h i j) = makeTupleString [ show a
                                                       , show b
                                                       , show c
                                                       , show d
                                                       , show e
                                                       , show f
                                                       , show g
                                                       , show h
                                                       , show i
                                                       , show j
                                                       ]

instance eqTuple10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (Tuple10 a b c d e f g h i j) where
  eq (Tuple10 a b c d e f g h i j) (Tuple10 a' b' c' d' e' f' g' h' i' j') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j'

uncurry10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Tuple10 a b c d e f g h i j -> k
uncurry10 fun (Tuple10 a b c d e f g h i j) = fun a b c d e f g h i j

curry10 :: forall a b c d e f g h i j k. (Tuple10 a b c d e f g h i j -> k) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
curry10 fun a b c d e f g h i j = fun (Tuple10 a b c d e f g h i j)


-- * Tuple11

data Tuple11 a b c d e f g h i j k = Tuple11 a b c d e f g h i j k

instance showTuple11 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (Tuple11 a b c d e f g h i j k) where
  show (Tuple11 a b c d e f g h i j k) = makeTupleString [ show a
                                                         , show b
                                                         , show c
                                                         , show d
                                                         , show e
                                                         , show f
                                                         , show g
                                                         , show h
                                                         , show i
                                                         , show j
                                                         , show k
                                                         ]

instance eqTuple11 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (Tuple11 a b c d e f g h i j k) where
  eq (Tuple11 a b c d e f g h i j k) (Tuple11 a' b' c' d' e' f' g' h' i' j' k') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k'

uncurry11 :: forall a b c d e f g h i j k l. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> Tuple11 a b c d e f g h i j k -> l
uncurry11 fun (Tuple11 a b c d e f g h i j k) = fun a b c d e f g h i j k

curry11 :: forall a b c d e f g h i j k l. (Tuple11 a b c d e f g h i j k -> l) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l
curry11 fun a b c d e f g h i j k = fun (Tuple11 a b c d e f g h i j k) 

-- * Tuple12

data Tuple12 a b c d e f g h i j k l = Tuple12 a b c d e f g h i j k l

instance showTuple12 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (Tuple12 a b c d e f g h i j k l) where
  show (Tuple12 a b c d e f g h i j k l) = makeTupleString [ show a
                                                           , show b
                                                           , show c
                                                           , show d
                                                           , show e
                                                           , show f
                                                           , show g
                                                           , show h
                                                           , show i
                                                           , show j
                                                           , show k
                                                           , show l
                                                           ]

instance eqTuple12 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (Tuple12 a b c d e f g h i j k l) where
  eq (Tuple12 a b c d e f g h i j k l) (Tuple12 a' b' c' d' e' f' g' h' i' j' k' l') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l'

uncurry12 :: forall a b c d e f g h i j k l m. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m) -> Tuple12 a b c d e f g h i j k l -> m
uncurry12 fun (Tuple12 a b c d e f g h i j k l) = fun a b c d e f g h i j k l

curry12 :: forall a b c d e f g h i j k l m. (Tuple12 a b c d e f g h i j k l -> m) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m
curry12 fun a b c d e f g h i j k = fun (Tuple12 a b c d e f g h i j k) 


-- * Tuple13

data Tuple13 a b c d e f g h i j k l m = Tuple13 a b c d e f g h i j k l m

instance showTuple13 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (Tuple13 a b c d e f g h i j k l m) where
  show (Tuple13 a b c d e f g h i j k l m) = makeTupleString [ show a
                                                             , show b
                                                             , show c
                                                             , show d
                                                             , show e
                                                             , show f
                                                             , show g
                                                             , show h
                                                             , show i
                                                             , show j
                                                             , show k
                                                             , show l
                                                             , show m
                                                             ]

instance eqTuple13 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (Tuple13 a b c d e f g h i j k l m) where
  eq (Tuple13 a b c d e f g h i j k l m) (Tuple13 a' b' c' d' e' f' g' h' i' j' k' l' m') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l' && m == m'

uncurry13 :: forall a b c d e f g h i j k l m n. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n) -> Tuple13 a b c d e f g h i j k l m -> n
uncurry13 fun (Tuple13 a b c d e f g h i j k l m) = fun a b c d e f g h i j k l m

curry13 :: forall a b c d e f g h i j k l m n. (Tuple13 a b c d e f g h i j k l m -> n) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n
curry13 fun a b c d e f g h i j k m = fun (Tuple13 a b c d e f g h i j k m) 


-- * Tuple14

data Tuple14 a b c d e f g h i j k l m n = Tuple14 a b c d e f g h i j k l m n

instance showTuple14 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (Tuple14 a b c d e f g h i j k l m n) where
  show (Tuple14 a b c d e f g h i j k l m n) = makeTupleString [ show a
                                                               , show b
                                                               , show c
                                                               , show d
                                                               , show e
                                                               , show f
                                                               , show g
                                                               , show h
                                                               , show i
                                                               , show j
                                                               , show k
                                                               , show l
                                                               , show m
                                                               , show n
                                                               ]

instance eqTuple14 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (Tuple14 a b c d e f g h i j k l m n) where
  eq (Tuple14 a b c d e f g h i j k l m n) (Tuple14 a' b' c' d' e' f' g' h' i' j' k' l' m' n') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l' && m == m' && n == n'

uncurry14 :: forall a b c d e f g h i j k l m n o. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o) -> Tuple14 a b c d e f g h i j k l m n -> o
uncurry14 fun (Tuple14 a b c d e f g h i j k l m n) = fun a b c d e f g h i j k l m n

curry14 :: forall a b c d e f g h i j k l m n o. (Tuple14 a b c d e f g h i j k l m n -> o) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o
curry14 fun a b c d e f g h i j k m n = fun (Tuple14 a b c d e f g h i j k m n) 

-- * Tuple15

data Tuple15 a b c d e f g h i j k l m n o = Tuple15 a b c d e f g h i j k l m n o
  
instance showTuple15 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (Tuple15 a b c d e f g h i j k l m n o) where
  show (Tuple15 a b c d e f g h i j k l m n o) = makeTupleString [ show a
                                                                 , show b
                                                                 , show c
                                                                 , show d
                                                                 , show e
                                                                 , show f
                                                                 , show g
                                                                 , show h
                                                                 , show i
                                                                 , show j
                                                                 , show k
                                                                 , show l
                                                                 , show m
                                                                 , show n
                                                                 , show o
                                                                 ]

instance eqTuple15 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (Tuple15 a b c d e f g h i j k l m n o) where
  eq (Tuple15 a b c d e f g h i j k l m n o) (Tuple15 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l' && m == m' && n == n' && o == o'

uncurry15 :: forall a b c d e f g h i j k l m n o p. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p) -> Tuple15 a b c d e f g h i j k l m n o -> p
uncurry15 fun (Tuple15 a b c d e f g h i j k l m n o) = fun a b c d e f g h i j k l m n o

curry15 :: forall a b c d e f g h i j k l m n o p. (Tuple15 a b c d e f g h i j k l m n o -> p) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p
curry15 fun a b c d e f g h i j k m n o = fun (Tuple15 a b c d e f g h i j k m n o) 


-- * Tuple16

data Tuple16 a b c d e f g h i j k l m n o p = Tuple16 a b c d e f g h i j k l m n o p
  
instance showTuple16 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) => Show (Tuple16 a b c d e f g h i j k l m n o p) where
  show (Tuple16 a b c d e f g h i j k l m n o p) = makeTupleString [ show a
                                                                   , show b
                                                                   , show c
                                                                   , show d
                                                                   , show e
                                                                   , show f
                                                                   , show g
                                                                   , show h
                                                                   , show i
                                                                   , show j
                                                                   , show k
                                                                   , show l
                                                                   , show m
                                                                   , show n
                                                                   , show o
                                                                   , show p
                                                                   ]

instance eqTuple16 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p) => Eq (Tuple16 a b c d e f g h i j k l m n o p) where
  eq (Tuple16 a b c d e f g h i j k l m n o p) (Tuple16 a' b' c' d' e' f' g' h' i' j' k' l' m' n' o' p') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f' && g == g' && h == h' && i == i' && j == j' && k == k' && l == l' && m == m' && n == n' && o == o' && p == p'

uncurry16 :: forall a b c d e f g h i j k l m n o p q. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q) -> Tuple16 a b c d e f g h i j k l m n o p -> q
uncurry16 fun (Tuple16 a b c d e f g h i j k l m n o p) = fun a b c d e f g h i j k l m n o p

curry16 :: forall a b c d e f g h i j k l m n o p q. (Tuple15 a b c d e f g h i j k l m n o p -> q) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> p -> q
curry16 fun a b c d e f g h i j k m n o p = fun (Tuple16 a b c d e f g h i j k m n o p) 



--------------------------------------------------------------------------------

makeTupleString :: Array String -> String
makeTupleString as = "(" <> joinWith ", " as <> ")"

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
