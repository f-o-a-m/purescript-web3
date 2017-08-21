module Network.Ethereum.Web3.Solidity.Tuple where

import Prelude
import Data.String (joinWith)
import Data.Array (reverse, (:))
import Data.Tuple (Tuple(..), snd)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Foldable (fold, foldMap)
import Type.Proxy (Proxy(..))
import Control.Monad.State.Class (get)
import Text.Parsing.Parser(Parser, ParseState(..))
import Text.Parsing.Parser.Combinators (lookAhead)
import Text.Parsing.Parser.Pos (Position(..))

import Network.Ethereum.Web3.Types (BigNumber, HexString, embed, hexLength, toInt)
import Network.Ethereum.Web3.Solidity.EncodingType (class EncodingType, isDynamic)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIEncoding, toDataBuilder, fromDataParser, take)

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

--------------------------------------------------------------------------------
newtype Singleton a = Singleton a

unSingleton :: forall a . Singleton a -> a
unSingleton (Singleton a) = a

instance showSingleton :: Show a => Show (Singleton a) where
  show (Singleton a) = makeTupleString [show a]

instance eqSingleton :: Eq a => Eq (Singleton a) where
  eq (Singleton a) (Singleton b) = a == b

uncurry1 :: forall a b . (a -> b) -> Singleton a -> b
uncurry1 f (Singleton a) = f a

curry1 :: forall a b . (Singleton a -> b) -> a -> b
curry1 f a = f (Singleton a)


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
uncurry2 f (Tuple2 a b) = f a b

curry2 :: forall a b c . (Tuple2 a b -> c) -> a -> b -> c
curry2 f a b = f (Tuple2 a b)

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
uncurry3 f (Tuple3 a b c) = f a b c

curry3 :: forall a b c d . (Tuple3 a b c -> d) -> a -> b -> c -> d
curry3 f a b c = f (Tuple3 a b c)

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
