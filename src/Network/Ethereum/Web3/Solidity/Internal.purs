module Network.Ethereum.Web3.Solidity.Internal
  ( class GRecordFieldsIso
  , gToRecordFields
  , gFromRecordFields
  , class ToRecordFields
  , toRecordFields
  , fromRecordFields
  , toRecord
  , fromRecord
  ) where

import Prelude

import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import Network.Ethereum.Web3.Solidity.Tuple (Tuple1(..), Tuple2(..))
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Type.RowList (class RowListAppend)
import Unsafe.Coerce (unsafeCoerce)

class GRecordFieldsIso :: forall k. RowList k -> Type -> Row Type -> Row Type -> Constraint
class GRecordFieldsIso xs rep from to | xs -> rep, rep -> xs, rep -> to, to rep -> from where
  gToRecordFields :: Proxy xs -> rep -> Builder { | from } { | to }
  gFromRecordFields :: Proxy xs -> Record to -> rep

instance GRecordFieldsIso RL.Nil NoArguments from from where
  gToRecordFields _ _ = identity
  gFromRecordFields _ _ = NoArguments

else instance (IsSymbol name, GRecordFieldsIso xs a from to) => GRecordFieldsIso (RL.Cons name xs RL.Nil) (Constructor name a) from to where
  gToRecordFields _ (Constructor a) = gToRecordFields (Proxy @xs) a
  gFromRecordFields _ r = Constructor (gFromRecordFields (Proxy @xs) r)

else instance
  ( GRecordFieldsIso xs a bto to
  , GRecordFieldsIso ys b from bto
  , RowListAppend xs ys zs
  ) =>
  GRecordFieldsIso (RL.Cons p q l) (Product a b) from to where
  gToRecordFields _ (Product as bs) =
    (gToRecordFields (Proxy @xs) as) <<< (gToRecordFields (Proxy @ys) bs)

  gFromRecordFields _ r =
    let
      as = gFromRecordFields (Proxy @xs) (unsafeCoerce r)
      bs = gFromRecordFields (Proxy @ys) (unsafeCoerce r)
    in
      Product as bs

else instance
  ( ToRecordFields xs a from to
  ) =>
  GRecordFieldsIso xs (Argument a) from to where
  gToRecordFields _ (Argument a) = toRecordFields (Proxy @xs) a
  gFromRecordFields _ r = Argument $ fromRecordFields (Proxy @xs) r

class ToRecordFields :: forall k. RowList k -> Type -> Row Type -> Row Type -> Constraint
class ToRecordFields xs a from to | xs -> a, a -> xs, from a -> to, a to -> from where
  toRecordFields :: Proxy xs -> a -> Builder { | from } { | to }
  fromRecordFields :: Proxy xs -> Record to -> a

instance (IsSymbol s, Row.Cons s a from to, Row.Lacks s from) => ToRecordFields (RL.Cons s a RL.Nil) (Tagged s a) from to where
  toRecordFields _ a = Builder.insert (Proxy @s) (untagged a)
  fromRecordFields _ r = tagged $ Record.get (Proxy @s) r

toRecord :: forall xs a rep fields. Generic a rep => GRecordFieldsIso xs rep () fields => a -> Record fields
toRecord a = Builder.buildFromScratch $ gToRecordFields (Proxy @xs) $ from a

fromRecord :: forall xs a rep row fields. Generic a rep => GRecordFieldsIso xs rep row fields => Record fields -> a
fromRecord a = to $ gFromRecordFields (Proxy @xs) a

y :: { a :: Int }
y = Builder.buildFromScratch $
  toRecordFields (Proxy @(RL.Cons "a" Int RL.Nil)) (tagged 1 :: Tagged "a" Int)

type T = RL.Cons "Tuple2" (RL.Cons "a" Int (RL.Cons "b" String RL.Nil)) RL.Nil

z :: { a :: Int, b :: String }
z =
  let
    a :: Tuple2 (Tagged "a" Int) (Tagged "b" String)
    a = Tuple2 (tagged 1 :: Tagged "a" Int) (tagged "hell" :: Tagged "b" String)
  --b = from a :: Int

  in
    Builder.buildFromScratch $ gToRecordFields (Proxy @T) $ from a
