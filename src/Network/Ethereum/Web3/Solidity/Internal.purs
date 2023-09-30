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
import Unsafe.Coerce (unsafeCoerce)

class GRecordFieldsIso rep fields | rep -> fields where
  gToRecordFields :: rep -> Record fields
  gFromRecordFields :: Record fields -> rep

instance GRecordFieldsIso NoArguments () where
  gToRecordFields _ = {}
  gFromRecordFields _ = NoArguments

else instance GRecordFieldsIso a r => GRecordFieldsIso (Constructor name a) r where
  gToRecordFields (Constructor a) = gToRecordFields a
  gFromRecordFields r = Constructor (gFromRecordFields r)

else instance
  ( IsSymbol s
  , Row.Cons s a () r
  , Row.Lacks s ()
  ) =>
  GRecordFieldsIso (Argument (Tagged s a)) r where
  gToRecordFields (Argument a) = Record.insert (Proxy @s) (untagged a) {}
  gFromRecordFields r = Argument (tagged $ Record.get (Proxy @s) r)

else instance
  ( GRecordFieldsIso as ra
  , GRecordFieldsIso bs rb
  , Row.Union ra rb r
  , Row.Nub r r
  ) =>
  GRecordFieldsIso (Product as bs) r where
  gToRecordFields (Product as bs) =
    Record.merge (gToRecordFields as) (gToRecordFields bs)

  gFromRecordFields r =
    let
      as = gFromRecordFields (unsafeCoerce r)
      bs = gFromRecordFields (unsafeCoerce r)
    in
      Product as bs

toRecord :: forall a rep fields. Generic a rep => GRecordFieldsIso rep fields => a -> Record fields
toRecord a = gToRecordFields $ from a

fromRecord :: forall a rep fields. Generic a rep => GRecordFieldsIso rep fields => Record fields -> a
fromRecord a = to $ gFromRecordFields a
