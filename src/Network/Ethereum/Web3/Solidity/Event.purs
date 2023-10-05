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

import Control.Error.Util (hush, note)
import Data.Array (uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), to)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.Tuple (Tuple(..))
import Debug (trace, traceM)
import Network.Ethereum.Types (HexString)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecodableValue, class GenericABIDecode, abiDecode, parseABIValue)
import Network.Ethereum.Web3.Solidity.Internal (class GRecordFieldsIso, toRecord)
import Network.Ethereum.Web3.Types (Change(..), Web3Error(..))
import Parsing (ParseError, fail)
import Partial.Unsafe (unsafeCrashWith)
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

instance ABIDecodableValue a => ArrayParser (Argument a) where
  arrayParser hxs = case uncons hxs of
    Nothing -> Left $ ParserError "no arguments found for arrayParser"
    Just { head, tail } -> do
      res <- lmap (ParserError <<< show) <<< parseABIValue $ head
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
  :: forall a b arep brep
   . Generic a arep
  => ArrayParser arep
  => Generic b brep
  => GenericABIDecode brep
  => Show a
  => Show b
  => Change
  -> Boolean
  -> Either Web3Error (Event a b)
parseChange (Change change) anonymous = do
  traceM "parseChange"
  topics <-
    if anonymous then pure change.topics
    else note (ParserError "no topics found") (_.tail <$> uncons change.topics)
  a <- genericArrayParser topics
  traceM ("a:\n" <> show a)
  let
    b = case abiDecode change.data :: Either _ b of
      Left err -> unsafeCrashWith ("err:\n" <> show err)
      Right res -> trace ("res:\n" <> show res) \_ -> res
  traceM ("b:\n" <> show b)
  pure $ Event a b

combineChange
  :: forall afields a arep bfields b brep c cfields
   . Generic a arep
  => Generic b brep
  => GRecordFieldsIso arep () afields
  => GRecordFieldsIso brep () bfields
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
  :: forall afields a arep bfields b brep c cfields
   . Generic a arep
  => GRecordFieldsIso arep () afields
  => GenericABIDecode arep
  => ArrayParser arep
  => GRecordFieldsIso brep () bfields
  => Generic b brep
  => GenericABIDecode brep
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
  let
    anonymous = isAnonymous (Proxy :: Proxy c)
  traceM (show change)
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
  , GRecordFieldsIso arep () afields
  , Generic a arep
  , GenericABIDecode arep
  , GRecordFieldsIso brep () bfields
  , Generic b brep
  , GenericABIDecode brep
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
