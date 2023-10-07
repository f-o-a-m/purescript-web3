module Network.Ethereum.Web3.Solidity.Internal
  ( class RecordFieldsIso
  , _toRecord
  , fromRecord
  , toRecord
  , class GRecordFieldsIso
  , gToRecord
  , gFromRecord
  ) where

import Prelude

import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol)
import Network.Ethereum.Web3.Solidity.Vector (Vector)
import Prim.Row as Row
import Record (disjointUnion)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class GRecordFieldsIso rep from to | from rep -> to, to rep -> from where
  gToRecord :: rep -> Builder { | from } { | to }
  gFromRecord :: Record to -> rep

instance GRecordFieldsIso NoArguments from from where
  gToRecord _ = identity
  gFromRecord _ = NoArguments

else instance
  ( IsSymbol name
  , GRecordFieldsIso a from to
  ) =>
  GRecordFieldsIso (Constructor name a) from to where
  gToRecord (Constructor a) = gToRecord a
  gFromRecord r = Constructor (gFromRecord r)

else instance
  ( GRecordFieldsIso a () ato
  , GRecordFieldsIso b () bto
  , Row.Union ato bto to
  , Row.Union to from to
  , Row.Nub to to
  ) =>
  GRecordFieldsIso (Product a b) from to where
  gToRecord (Product as bs) =
    let
      r = Builder.buildFromScratch (gToRecord as) `disjointUnion` Builder.buildFromScratch (gToRecord bs)
    in
      Builder.merge r

  gFromRecord r =
    let
      as = gFromRecord (unsafeCoerce r :: Record ato)
      bs = gFromRecord (unsafeCoerce r :: Record bto)
    in
      Product as bs

else instance
  ( RecordFieldsIso a from to
  ) =>
  GRecordFieldsIso (Argument a) from to where
  gToRecord (Argument a) = _toRecord a
  gFromRecord r = Argument $ fromRecord r

class RecordFieldsIso a from to | from a -> to, a to -> from where
  _toRecord :: a -> Builder { | from } { | to }
  fromRecord :: Record to -> a

instance
  ( IsSymbol s
  , Row.Cons s (Array (Record to)) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  ) =>
  RecordFieldsIso (Tagged s (Array a)) from to' where
  _toRecord a =
    Builder.insert (Proxy @s) $ map
      (Builder.buildFromScratch <<< (gToRecord <<< from))
      (untagged a)
  fromRecord r =
    tagged $ map (to <<< gFromRecord) $ Record.get (Proxy @s) r

else instance
  ( IsSymbol s
  , Row.Cons s (Vector n (Record to)) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  ) =>
  RecordFieldsIso (Tagged s (Vector n a)) from to' where
  _toRecord a =
    Builder.insert (Proxy @s) $ map
      (Builder.buildFromScratch <<< (gToRecord <<< from))
      (untagged a)
  fromRecord r =
    tagged $ map (to <<< gFromRecord) $ Record.get (Proxy @s) r

else instance
  ( IsSymbol s
  , Row.Cons s a from to
  , Row.Lacks s from
  ) =>
  RecordFieldsIso (Tagged s (Identity a)) from to where
  _toRecord a = Builder.insert (Proxy @s) (un Identity $ untagged a)
  fromRecord r = tagged $ Identity $ Record.get (Proxy @s) r

else instance
  ( IsSymbol s
  , Row.Cons s (Record to) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  ) =>
  RecordFieldsIso (Tagged s a) from to' where
  _toRecord a = Builder.insert (Proxy @s) $
    Builder.buildFromScratch (gToRecord $ from $ untagged a)
  fromRecord r = tagged $ to $ gFromRecord $ Record.get (Proxy @s) r

else instance
  ( Generic a arep
  , GRecordFieldsIso arep from to
  ) =>
  RecordFieldsIso a from to where
  _toRecord a =
    gToRecord $ from a
  fromRecord r =
    to $ gFromRecord r

toRecord
  :: forall a fields
   . RecordFieldsIso a () fields
  => a
  -> Record fields
toRecord a =
  Builder.buildFromScratch $ _toRecord a
