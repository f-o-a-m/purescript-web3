module Network.Ethereum.Web3.Solidity.Internal
  ( class GRecordFieldsIso
  , gToRecordFields
  , gFromRecordFields
  , toRecord
  , fromRecord
  ) where

import Prelude

import Data.Functor.Tagged (Tagged, untagged, tagged)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Type.RowList (class RowListAppend, Cons, Nil)
import Unsafe.Coerce (unsafeCoerce)

class GRecordFieldsIso :: forall k1. Type -> Row Type -> k1 -> Constraint
class GRecordFieldsIso rep fields k | rep -> k, k -> rep fields where
  gToRecordFields :: Proxy k -> rep -> Record fields
  gFromRecordFields :: Proxy k -> Record fields -> rep

instance GRecordFieldsIso NoArguments () Nil where
  gToRecordFields _ _ = {}
  gFromRecordFields _ _ = NoArguments

else instance
  ( IsSymbol s
  , Row.Cons s a () r
  , Row.Lacks s ()
  ) =>
  GRecordFieldsIso (Argument (Tagged s a)) r (Cons s a Nil) where
  gToRecordFields _ (Argument a) = Record.insert (Proxy @s) (untagged a) {}
  gFromRecordFields _ r = Argument (tagged $ Record.get (Proxy @s) r)

else instance
  ( GRecordFieldsIso as ra rla
  , GRecordFieldsIso bs rb rlb
  , RowListAppend rla rlb rl
  , Row.Union ra rb r
  , Row.Nub r r
  ) =>
  GRecordFieldsIso (Product as bs) r rl where
  gToRecordFields _ (Product as bs) =
    Record.merge (gToRecordFields (Proxy @rla) as) (gToRecordFields (Proxy @rlb) bs)

  gFromRecordFields _ r =
    let
      as = gFromRecordFields (Proxy @rla) (unsafeCoerce r)
      bs = gFromRecordFields (Proxy @rlb) (unsafeCoerce r)
    in
      Product as bs

else instance GRecordFieldsIso a r rl => GRecordFieldsIso (Constructor name a) r (Cons name rl Nil) where
  gToRecordFields _ (Constructor a) = gToRecordFields (Proxy @rl) a
  gFromRecordFields _ r = Constructor (gFromRecordFields (Proxy @rl) r)

toRecord :: forall a rep fields l. Generic a rep => GRecordFieldsIso rep fields l => a -> Record fields
toRecord a = gToRecordFields (Proxy :: Proxy l) (from a)

fromRecord :: forall a rep fields l. Generic a rep => GRecordFieldsIso rep fields l => Record fields -> a
fromRecord a = to $ gFromRecordFields (Proxy :: Proxy l) a
