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
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, fromData)
import Network.Ethereum.Web3.Solidity.Generic (class GenericABIDecode, class RecordFieldsIso, genericFromData, genericToRecordFields)
import Network.Ethereum.Web3.Types (Change(..))
import Prim.Row as Row
import Record.Builder (build, merge)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
-- Array Parsers
--------------------------------------------------------------------------------

-- TODO: rename to `GenericArrayABIDecode`
class ArrayParser a where
-- TODO: rename to `genericArrayABIDecodeImplementation`
  arrayParser :: Array HexString -> Maybe a

instance arrayParserNoArgs :: ArrayParser NoArguments where
  arrayParser _ = Just NoArguments

instance arrayParserBase :: ABIDecode a => ArrayParser (Argument a) where
  arrayParser hxs = case uncons hxs of
    Nothing -> Nothing
    Just { head } -> map Argument <<< hush <<< fromData $ head

instance arrayParserInductive :: (ArrayParser as, ABIDecode a) => ArrayParser (Product (Argument a) as) where
  arrayParser hxs = case uncons hxs of
    Nothing -> Nothing
    Just { head, tail } -> Product <$> (map Argument <<< hush <<< fromData $ head) <*> arrayParser tail

instance arrayParserConstructor :: ArrayParser as => ArrayParser (Constructor name as) where
  arrayParser = map Constructor <<< arrayParser

-- e.g. parses
-- ["0x0000....", "0xfffff..."]
-- to `Maybe (Tuple2 (Tagged (Proxy "xxx") Address) (Tagged (Proxy "yyy") Address))`
-- TODO: rename to `genericArrayABIDecode`
genericArrayParser ::
  forall a rep.
  Generic a rep =>
  ArrayParser rep =>
  Array HexString ->
  Maybe a
genericArrayParser = map to <<< arrayParser

--------------------------------------------------------------------------------
-- | Event Parsers
--------------------------------------------------------------------------------
data Event indexedData nonIndexedData
  = Event indexedData nonIndexedData

parseChange ::
  forall a b arep brep.
  Generic a arep =>
  ArrayParser arep =>
  Generic b brep =>
  GenericABIDecode brep =>
  Change ->
  Boolean ->
  Maybe (Event a b)
parseChange (Change change) anonymous = do
  -- from https://docs.soliditylang.org/en/latest/abi-spec.html#events
  -- if event is `anonymous` (i.e. has `anonymous` keyword in `event MyEvent(uint256 indexed foo, uint256 bar) anonymous`)
  -- then `topics[0]` is `keccak(EVENT_NAME+"("+EVENT_ARGS.map(canonical_type_of).join(",")+")")`
  -- example - https://gist.github.com/srghma/c8e92bf400d65939f2c7e035199500c8
  (topics :: Array HexString) <- if anonymous then pure change.topics else _.tail <$> uncons change.topics -- e.g. ['0000...', 'ffff...']
  indexedData <- genericArrayParser topics -- e.g. `Tuple2 (Tagged (Proxy "foo") BigNumber) (Tagged (Proxy "bar") Address)`
  nonIndexedData <- hush <<< genericFromData $ change.data -- e.g. `Tuple2 (Tagged (Proxy "foo") BigNumber) (Tagged (Proxy "bar") Address)`
  pure $ Event indexedData nonIndexedData

combineChange ::
  forall
  a_genericTaggedRepresentation a_recordRow a_recordRowList (a :: Type) a_tupleNName
  b_genericTaggedRepresentation b_recordRow b_recordRowList (b :: Type) b_tupleNName
  c c_recordRow c_recordRowNubbed.
  RecordFieldsIso a_genericTaggedRepresentation a_recordRow a_recordRowList =>
  Generic a (Constructor a_tupleNName a_genericTaggedRepresentation) =>
  RecordFieldsIso b_genericTaggedRepresentation b_recordRow b_recordRowList =>
  Generic b (Constructor b_tupleNName b_genericTaggedRepresentation) =>
  Row.Union a_recordRow b_recordRow c_recordRow =>
  Row.Nub c_recordRow c_recordRowNubbed =>
  Newtype c (Record c_recordRowNubbed) =>
  Event a b ->
  c
combineChange (Event a b) = wrap $ build (merge (genericToRecordFields a)) (genericToRecordFields b)

class IndexedEvent :: forall indexedTypesTaggedK nonIndexedTypesTaggedK constructorK. indexedTypesTaggedK -> nonIndexedTypesTaggedK -> constructorK -> Constraint
class IndexedEvent indexedTypesTagged nonIndexedTypesTagged constructor | constructor -> indexedTypesTagged nonIndexedTypesTagged where
  isAnonymous :: Proxy constructor -> Boolean

decodeEventDef ::
  forall
  a_genericTaggedRepresentation a_recordRow a_recordRowList a a_tupleNName
  b_genericTaggedRepresentation b_recordRow b_recordRowList b b_tupleNName
  c c_recordRow c_recordRowNubbed.
  ArrayParser a_genericTaggedRepresentation =>
  RecordFieldsIso a_genericTaggedRepresentation a_recordRow a_recordRowList =>
  Generic a (Constructor a_tupleNName a_genericTaggedRepresentation) =>
  RecordFieldsIso b_genericTaggedRepresentation b_recordRow b_recordRowList =>
  Generic b (Constructor b_tupleNName b_genericTaggedRepresentation) =>
  GenericABIDecode b_genericTaggedRepresentation =>
  Row.Union a_recordRow b_recordRow c_recordRow =>
  Row.Nub c_recordRow c_recordRowNubbed =>
  Newtype c (Record c_recordRowNubbed) =>
  IndexedEvent a b c =>
  Change ->
  Maybe c
decodeEventDef change = do
  let
    anonymous = isAnonymous (Proxy :: Proxy c)
  (e :: Event a b) <- parseChange change anonymous
  pure $ combineChange e

class DecodeEvent :: forall k1 k2. k1 -> k2 -> Type -> Constraint
class
  IndexedEvent a b c <= DecodeEvent a b c | c -> a b where
  decodeEvent :: Change -> Maybe c

instance defaultInstance ::
  ( ArrayParser a_genericTaggedRepresentation
  , RecordFieldsIso a_genericTaggedRepresentation a_recordRow a_recordRowList
  , Generic a (Constructor a_tupleNName a_genericTaggedRepresentation)
  , RecordFieldsIso b_genericTaggedRepresentation b_recordRow b_recordRowList
  , Generic b (Constructor b_tupleNName b_genericTaggedRepresentation)
  , GenericABIDecode b_genericTaggedRepresentation
  , Row.Union a_recordRow b_recordRow c_recordRow
  , Row.Nub c_recordRow c_recordRowNubbed
  , Newtype c (Record c_recordRowNubbed)
  , IndexedEvent a b c
  ) =>
  DecodeEvent a b c where
  decodeEvent = decodeEventDef
