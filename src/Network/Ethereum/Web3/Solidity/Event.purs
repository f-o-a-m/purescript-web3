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

import Control.Error.Util (hush)
import Data.Array (uncons)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), to)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Network.Ethereum.Types (HexString)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecodableValue, class GenericABIDecode, abiDecode, parseABIValue)
import Network.Ethereum.Web3.Solidity.Internal (class GRecordFieldsIso, toRecord)
import Network.Ethereum.Web3.Types (Change(..))
import Prim.Row as Row
import Record (disjointUnion)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Array Parsers
--------------------------------------------------------------------------------
class ArrayParser a where
  arrayParser :: Array HexString -> Maybe a

instance ArrayParser NoArguments where
  arrayParser _ = Just NoArguments

instance ABIDecodableValue a => ArrayParser (Argument a) where
  arrayParser hxs = case uncons hxs of
    Nothing -> Nothing
    Just { head } -> map Argument <<< hush <<< parseABIValue $ head

instance arrayParserInductive :: (ArrayParser as, ABIDecodableValue a) => ArrayParser (Product (Argument a) as) where
  arrayParser hxs = case uncons hxs of
    Nothing -> Nothing
    Just { head, tail } -> Product <$> (map Argument <<< hush <<< parseABIValue $ head) <*> arrayParser tail

instance arrayParserConstructor :: ArrayParser as => ArrayParser (Constructor name as) where
  arrayParser = map Constructor <<< arrayParser

genericArrayParser
  :: forall a rep
   . Generic a rep
  => ArrayParser rep
  => Array HexString
  -> Maybe a
genericArrayParser = map to <<< arrayParser

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
  => Change
  -> Boolean
  -> Maybe (Event a b)
parseChange (Change change) anonymous = do
  topics <- if anonymous then pure change.topics else _.tail <$> uncons change.topics
  a <- genericArrayParser topics
  b <- hush <<< abiDecode $ change.data
  pure $ Event a b

combineChange
  :: forall afields xs ys a arep bfields b brep c cfields
   . Generic a arep
  => Generic b brep
  => GRecordFieldsIso xs arep () afields
  => GRecordFieldsIso ys brep () bfields
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

decodeEventDef
  :: forall xs ys afields a arep bfields b brep c cfields
   . Generic a arep
  => GRecordFieldsIso xs arep () afields
  => GenericABIDecode arep
  => ArrayParser arep
  => GRecordFieldsIso ys brep () bfields
  => Generic b brep
  => GenericABIDecode brep
  => Row.Union afields bfields cfields
  => Row.Nub cfields cfields
  => Newtype c (Record cfields)
  => IndexedEvent a b c
  => Change
  -> Maybe c
decodeEventDef change = do
  let
    anonymous = isAnonymous (Proxy :: Proxy c)
  (e :: Event a b) <- parseChange change anonymous
  pure $ combineChange e

class DecodeEvent :: forall k1 k2. k1 -> k2 -> Type -> Constraint
class
  IndexedEvent a b c <=
  DecodeEvent a b c
  | c -> a b where
  decodeEvent :: Change -> Maybe c

instance
  ( ArrayParser arep
  , GRecordFieldsIso xs arep () afields
  , Generic a arep
  , GenericABIDecode arep
  , GRecordFieldsIso xs brep () bfields
  , Generic b brep
  , GenericABIDecode brep
  , Row.Union afields bfields cfields
  , Row.Nub cfields cfields
  , Newtype c (Record cfields)
  , IndexedEvent a b c
  ) =>
  DecodeEvent a b c where
  decodeEvent = decodeEventDef
