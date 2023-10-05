module Network.Ethereum.Web3.Solidity.Event
  ( class DecodeEvent
  , decodeEvent
  , decodeEventDef
  , class ArrayParser
  , arrayParser
  , genericArrayParser
  , class IndexedEvent
  , isAnonymous
  ) where

import Prelude

import Control.Error.Util (note)
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), to)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Types (HexString)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, class ABIEncode, abiDecode)
import Network.Ethereum.Web3.Solidity.Internal (class RecordFieldsIso, toRecord)
import Network.Ethereum.Web3.Types (Change(..), Web3Error(..))
import Prim.Row as Row
import Record (disjointUnion)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Array Parsers
--------------------------------------------------------------------------------
class ArrayParser a where
  arrayParser :: Array HexString -> Either Web3Error (Tuple a (Array HexString))

instance ArrayParser NoArguments where
  arrayParser as = pure (Tuple NoArguments as)

instance ABIDecode a => ArrayParser (Argument a) where
  arrayParser hxs = case uncons hxs of
    Nothing -> Left $ ParserError "no arguments found for arrayParser"
    Just { head, tail } -> do
      res <- lmap (ParserError <<< show) <<< abiDecode $ head
      pure $ Tuple (Argument res) tail

instance (ArrayParser as, ArrayParser bs) => ArrayParser (Product as bs) where
  arrayParser hxs = do
    Tuple a rest <- arrayParser hxs
    Tuple b rest' <- arrayParser rest
    pure $ Tuple (Product a b) rest'

instance ArrayParser as => ArrayParser (Constructor name as) where
  arrayParser hxs = do
    Tuple a rest <- arrayParser hxs
    pure $ Tuple (Constructor a) rest

genericArrayParser
  :: forall a rep
   . Generic a rep
  => ArrayParser rep
  => Array HexString
  -> Either Web3Error a
genericArrayParser hxs = do
  Tuple a rest <- arrayParser hxs
  case rest of
    [] -> pure $ to a
    _ -> Left $ ParserError "too many arguments to arrayParser"

--------------------------------------------------------------------------------
-- | Event Parsers
--------------------------------------------------------------------------------
data Event i ni = Event i ni

parseChange
  :: forall a b arep
   . Generic a arep
  => ArrayParser arep
  => ABIDecode b
  => Show a
  => Show b
  => Change
  -> Boolean
  -> Either Web3Error (Event a b)
parseChange (Change change) anonymous = do
  topics <-
    if anonymous then pure change.topics
    else note (ParserError "no topics found") (_.tail <$> uncons change.topics)
  a <- genericArrayParser topics
  b <- lmap (ParserError <<< show) $ abiDecode change.data
  pure $ Event a b

combineChange
  :: forall afields a bfields b c cfields
   . RecordFieldsIso a () afields
  => RecordFieldsIso b () bfields
  => Row.Union afields bfields cfields
  => Row.Nub cfields cfields
  => Newtype c (Record cfields)
  => Show a
  => Show b
  => Event a b
  -> c
combineChange (Event a b) =
  wrap $ disjointUnion (toRecord a :: Record afields) (toRecord b :: Record bfields)

class IndexedEvent :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class IndexedEvent a b c | c -> a b where
  isAnonymous :: Proxy c -> Boolean

decodeEventDef
  :: forall afields a arep bfields b c cfields
   . Generic a arep
  => RecordFieldsIso a () afields
  => ABIEncode a
  => ArrayParser arep
  => RecordFieldsIso b () bfields
  => ABIDecode b
  => Row.Union afields bfields cfields
  => Row.Nub cfields cfields
  => Show a
  => Show b
  => Show c
  => Newtype c (Record cfields)
  => IndexedEvent a b c
  => Change
  -> Either Web3Error c
decodeEventDef change = do
  let anonymous = isAnonymous (Proxy :: Proxy c)
  (e :: Event a b) <- parseChange change anonymous
  pure $ combineChange e

class DecodeEvent :: forall k1 k2. k1 -> k2 -> Type -> Constraint
class
  IndexedEvent a b c <=
  DecodeEvent a b c
  | c -> a b where
  decodeEvent :: Change -> Either Web3Error c

instance
  ( ArrayParser arep
  , RecordFieldsIso a () afields
  , ABIEncode a
  , Generic a arep
  , RecordFieldsIso b () bfields
  , ABIDecode b
  , Row.Union afields bfields cfields
  , Row.Nub cfields cfields
  , Newtype c (Record cfields)
  , IndexedEvent a b c
  , Show a
  , Show b
  , Show c
  ) =>
  DecodeEvent a b c where
  decodeEvent = decodeEventDef
