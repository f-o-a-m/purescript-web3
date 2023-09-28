module Network.Ethereum.Web3.Solidity.Internal where

import Prelude

import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow, Cons, Nil, RowList)

class ArgsToRowListProxy :: forall k. k -> RowList Type -> Constraint
class ArgsToRowListProxy args l | args -> l, l -> args where
  argsToRowListProxy :: Proxy args -> Proxy l

instance argsToRowListProxyBaseNull :: ArgsToRowListProxy NoArguments Nil where
  argsToRowListProxy _ = Proxy

instance argsToRowListProxyBase :: ArgsToRowListProxy (Argument (Tagged (Proxy s) a)) (Cons s a Nil) where
  argsToRowListProxy _ = Proxy
else instance argsToRowListProxyInductive :: ArgsToRowListProxy as l => ArgsToRowListProxy (Product (Argument (Tagged (Proxy s) a)) as) (Cons s a l) where
  argsToRowListProxy _ = Proxy

class RecordFieldsIso args fields (rowList :: RowList Type) | args -> rowList, rowList -> args fields where
  toRecordFields :: forall proxy. proxy rowList -> args -> Record fields
  fromRecordFields :: forall proxy. proxy rowList -> Record fields -> args

instance isoRecordBase ::
  ( IsSymbol s
  , Row.Cons s a () r
  , Row.Lacks s ()
  ) =>
  RecordFieldsIso (Argument (Tagged s a)) r (Cons s a Nil) where
  toRecordFields _ (Argument a) = Record.insert (Proxy :: Proxy s) (untagged a) {}
  fromRecordFields _ r = Argument (tagged $ Record.get (Proxy :: Proxy s) r)

instance isoRecordBaseNull :: RecordFieldsIso NoArguments () Nil where
  toRecordFields _ _ = {}
  fromRecordFields _ _ = NoArguments

instance isoRecordInductive ::
  ( RecordFieldsIso as r1 (Cons ls la ll)
  , Row.Cons s a r1 r2
  , Row.Lacks s r1
  , IsSymbol s
  , ListToRow (Cons ls la ll) r1
  ) =>
  RecordFieldsIso (Product (Argument (Tagged s a)) as) r2 (Cons s a (Cons ls la ll)) where
  toRecordFields _ (Product (Argument a) as) = Record.insert (Proxy :: Proxy s) (untagged a) rest
    where
    rest = (toRecordFields (Proxy :: Proxy (Cons ls la ll)) as :: Record r1)
  fromRecordFields _ r =
    let
      a = Argument (tagged $ Record.get (Proxy :: Proxy s) r)

      before = Record.delete (Proxy :: Proxy s) r :: Record r1

      rest = fromRecordFields (Proxy :: Proxy (Cons ls la ll)) before
    in
      Product a rest

genericToRecordFields
  :: forall args fields l a name
   . RecordFieldsIso args fields l
  => Generic a (Constructor name args)
  => a
  -> Record fields
genericToRecordFields a =
  let
    Constructor row = from a
  in
    toRecordFields (Proxy :: Proxy l) row

genericFromRecordFields
  :: forall args fields l a name
   . RecordFieldsIso args fields l
  => Generic a (Constructor name args)
  => Record fields
  -> a
genericFromRecordFields r = to $ Constructor $ fromRecordFields (Proxy :: Proxy l) r
