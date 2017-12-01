module Network.Ethereum.Web3.Solidity.Event where

import Prelude
import Type.Equality

import Data.Generic.Rep (class Generic, Constructor)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Record.Builder (build, merge)
import Network.Ethereum.Web3.Solidity.AbiEncoding (class ABIDecode, fromData)
import Network.Ethereum.Web3.Solidity.Generic (class ArgsToRowListProxy, class ArrayParser, class GenericABIDecode, class ToRecordFields, arrayParser, genericFromData, genericParseArray, genericToRecordFields)
import Network.Ethereum.Web3.Types (Change(..))
import Type.Row (class ListToRow)


data Event i ni = Event i ni

parseChange' :: forall a b arep brep.
                Generic a arep
             => ArrayParser arep
             => Generic b brep
             => GenericABIDecode brep
             => Change
             -> Maybe (Event a b)
parseChange' (Change change) = do
  a <- genericParseArray change.topics
  b <- genericFromData change.data
  pure $ Event a b


combineChange :: forall aargs afields al a aname bargs bfields bl b bname c cfields.
                 ToRecordFields aargs afields al
              => Generic a (Constructor aname aargs)
              => ArgsToRowListProxy aargs al
              => ListToRow al afields
              => ToRecordFields bargs bfields bl
              => Generic b (Constructor bname bargs)
              => ArgsToRowListProxy bargs bl
              => ListToRow bl bfields
              => Union bfields afields cfields
              => Newtype c (Record cfields)
              => Event a b
              -> c
combineChange (Event a b) = wrap $ build (merge (genericToRecordFields a)) (genericToRecordFields b)


class DecodeEvent a b c | c -> a b

decodeEvent :: forall aargs afields al a aname bargs bfields bl b bname c cfields.
               ArrayParser aargs
            => ToRecordFields aargs afields al
            => Generic a (Constructor aname aargs)
            => ArgsToRowListProxy aargs al
            => ListToRow al afields
            => ToRecordFields bargs bfields bl
            => Generic b (Constructor bname bargs)
            => GenericABIDecode bargs
            => ArgsToRowListProxy bargs bl
            => ListToRow bl bfields
            => Union bfields afields cfields
            => Newtype c (Record cfields)
            => DecodeEvent a b c
            => Change
            -> Maybe c
decodeEvent change = do
  (e :: Event a b) <- parseChange' change
  pure $ (combineChange :: Event a b -> c) e
