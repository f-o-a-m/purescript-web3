module Network.Ethereum.Web3.Solidity.Event
  ( class DecodeEvent
  , decodeEvent
  , class ArrayParser
  , arrayParser
  , class GArrayParser
  , gArrayParser
  , class IndexedEvent
  , isAnonymous
  ) where

import Prelude

import Control.Error.Util (note)
import Control.Monad.Error.Class (throwError)
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

instance (Generic a rep, GArrayParser rep) => ArrayParser a where
  arrayParser hx = do
    Tuple a rest <- gArrayParser hx
    case rest of
      [] -> pure $ Tuple (to a) rest
      _ -> throwError $ ParserError "too many arguments to arrayParser"

class GArrayParser rep where
  gArrayParser :: Array HexString -> Either Web3Error (Tuple rep (Array HexString))

instance GArrayParser NoArguments where
  gArrayParser as = pure (Tuple NoArguments as)

else instance ABIDecode a => GArrayParser (Argument a) where
  gArrayParser hxs = case uncons hxs of
    Nothing -> Left $ ParserError "no arguments found for arrayParser"
    Just { head, tail } -> do
      res <- lmap (ParserError <<< show) <<< abiDecode $ head
      pure $ Tuple (Argument res) tail

else instance (ArrayParser as, ArrayParser bs) => GArrayParser (Product as bs) where
  gArrayParser hxs = do
    Tuple a rest <- arrayParser hxs
    Tuple b rest' <- arrayParser rest
    pure $ Tuple (Product a b) rest'

else instance ArrayParser as => GArrayParser (Constructor name as) where
  gArrayParser hxs = do
    Tuple a rest <- arrayParser hxs
    pure $ Tuple (Constructor a) rest

--------------------------------------------------------------------------------
-- | Event Parsers
--------------------------------------------------------------------------------
data Event i ni = Event i ni

parseChange
  :: forall a b
   . ArrayParser a
  => ABIDecode b
  => Change
  -> Boolean
  -> Either Web3Error (Event a b)
parseChange (Change change) anonymous = do
  topics <-
    if anonymous then pure change.topics
    else note (ParserError "no topics found") (_.tail <$> uncons change.topics)
  Tuple a _ <- arrayParser topics
  b <- lmap (ParserError <<< show) $ abiDecode change.data
  pure $ Event a b

combineChange
  :: forall afields a bfields b c cfields
   . RecordFieldsIso a () afields
  => RecordFieldsIso b () bfields
  => Row.Union afields bfields cfields
  => Row.Nub cfields cfields
  => Newtype c (Record cfields)
  => Event a b
  -> c
combineChange (Event a b) =
  wrap $ disjointUnion (toRecord a :: Record afields) (toRecord b :: Record bfields)

class IndexedEvent :: forall k1 k2 k3. k1 -> k2 -> k3 -> Constraint
class IndexedEvent a b c | c -> a b where
  isAnonymous :: Proxy c -> Boolean

class DecodeEvent :: forall k1 k2. k1 -> k2 -> Type -> Constraint
class
  IndexedEvent a b c <=
  DecodeEvent a b c
  | c -> a b where
  decodeEvent :: Change -> Either Web3Error c

instance
  ( ArrayParser a
  , RecordFieldsIso a () afields
  , ABIEncode a
  , RecordFieldsIso b () bfields
  , ABIDecode b
  , Row.Union afields bfields cfields
  , Row.Nub cfields cfields
  , Newtype c (Record cfields)
  , IndexedEvent a b c
  ) =>
  DecodeEvent a b c where
  decodeEvent change = do
    let anonymous = isAnonymous (Proxy :: Proxy c)
    (e :: Event a b) <- parseChange change anonymous
    pure $ combineChange e
