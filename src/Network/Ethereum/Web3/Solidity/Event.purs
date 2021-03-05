module Network.Ethereum.Web3.Solidity.Event
  ( class DecodeEvent
  , decodeEvent
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
class ArrayParser a where
  arrayParser :: Array HexString -> Maybe a

instance arrayParserNoArgs :: ArrayParser NoArguments where
  arrayParser hxs = Just NoArguments

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
data Event i ni
  = Event i ni

parseChange ::
  forall a b arep brep.
  Generic a arep =>
  ArrayParser arep =>
  Generic b brep =>
  GenericABIDecode brep =>
  Change ->
  Boolean -- is anonymous ->
    Maybe
    (Event a b)
parseChange (Change change) anonymous = do
  topics <- if anonymous then pure change.topics else _.tail <$> uncons change.topics
  a <- genericArrayParser topics
  b <- hush <<< genericFromData $ change.data
  pure $ Event a b

combineChange ::
  forall aargs afields al a aname bargs bfields bl b bname c cfields cfieldsRes.
  RecordFieldsIso aargs afields al =>
  Generic a (Constructor aname aargs) =>
  RecordFieldsIso bargs bfields bl =>
  Generic b (Constructor bname bargs) =>
  Row.Union bfields afields cfields =>
  Row.Nub cfields cfieldsRes =>
  Newtype c (Record cfieldsRes) =>
  Event a b ->
  c
combineChange (Event a b) = wrap $ build (merge (genericToRecordFields a)) (genericToRecordFields b)

class IndexedEvent a b c | c -> a b where
  isAnonymous :: Proxy c -> Boolean

decodeEventDef ::
  forall aargs afields al a aname bargs bfields bl b bname c cfields cfieldsRes.
  ArrayParser aargs =>
  RecordFieldsIso aargs afields al =>
  Generic a (Constructor aname aargs) =>
  RecordFieldsIso bargs bfields bl =>
  Generic b (Constructor bname bargs) =>
  GenericABIDecode bargs =>
  Row.Union bfields afields cfields =>
  Row.Nub cfields cfieldsRes =>
  Newtype c (Record cfieldsRes) =>
  IndexedEvent a b c =>
  Change ->
  Maybe c
decodeEventDef change = do
  let
    anonymous = isAnonymous (Proxy :: Proxy c)
  (e :: Event a b) <- parseChange change anonymous
  pure $ combineChange e

class
  IndexedEvent a b c <= DecodeEvent a b c | c -> a b where
  decodeEvent :: Change -> Maybe c

instance defaultInstance ::
  ( ArrayParser aargs
  , RecordFieldsIso aargs afields al
  , Generic a (Constructor aname aargs)
  , RecordFieldsIso bargs bfields bl
  , Generic b (Constructor bname bargs)
  , GenericABIDecode bargs
  , Row.Union bfields afields cfields
  , Row.Nub cfields cfieldsRes
  , Newtype c (Record cfieldsRes)
  , IndexedEvent a b c
  ) =>
  DecodeEvent a b c where
  decodeEvent = decodeEventDef
