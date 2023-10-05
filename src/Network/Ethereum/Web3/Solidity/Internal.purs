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
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class GRecordFieldsIso rep from to | rep -> to, to rep -> from where
  gToRecordFields :: rep -> Builder { | from } { | to }
  gFromRecordFields :: Record to -> rep

instance GRecordFieldsIso NoArguments from from where
  gToRecordFields _ = identity
  gFromRecordFields _ = NoArguments

else instance (IsSymbol name, GRecordFieldsIso a from to) => GRecordFieldsIso (Constructor name a) from to where
  gToRecordFields (Constructor a) = gToRecordFields a
  gFromRecordFields r = Constructor (gFromRecordFields r)

else instance
  ( GRecordFieldsIso a bto to
  , GRecordFieldsIso b from bto
  ) =>
  GRecordFieldsIso (Product a b) from to where
  gToRecordFields (Product as bs) = gToRecordFields as <<< gToRecordFields bs

  gFromRecordFields r =
    let
      as = gFromRecordFields (unsafeCoerce r)
      bs = gFromRecordFields (unsafeCoerce r)
    in
      Product as bs

else instance
  ( ToRecordFields a from to
  ) =>
  GRecordFieldsIso (Argument a) from to where
  gToRecordFields (Argument a) = toRecordFields a
  gFromRecordFields r = Argument $ fromRecordFields r

class ToRecordFields a from to | from a -> to, a to -> from where
  toRecordFields :: a -> Builder { | from } { | to }
  fromRecordFields :: Record to -> a

instance
  ( IsSymbol s
  , Row.Cons s (Array (Record to)) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  ) =>
  ToRecordFields (Tagged s (Array a)) from to' where
  toRecordFields a = Builder.insert (Proxy @s) $ map
    (Builder.buildFromScratch <<< (gToRecordFields <<< from))
    (untagged a)
  fromRecordFields r = tagged $ map (to <<< gFromRecordFields) $ Record.get (Proxy @s) r

else instance foo ::
  ( IsSymbol s
  , Row.Cons s a from to
  , Row.Lacks s from
  ) =>
  ToRecordFields (Tagged s (Identity a)) from to where
  toRecordFields a = Builder.insert (Proxy @s) (un Identity $ untagged a)
  fromRecordFields r = tagged $ Identity $ Record.get (Proxy @s) r

else instance bar ::
  ( IsSymbol s
  , Row.Cons s (Record to) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  ) =>
  ToRecordFields (Tagged s a) from to' where
  toRecordFields a = Builder.insert (Proxy @s)
    $ Builder.buildFromScratch (gToRecordFields $ from $ untagged a)
  fromRecordFields r = tagged $ to $ gFromRecordFields $ Record.get (Proxy @s) r

--else instance ToRecordFields (Record r) r r where
--  toRecordFields _ = identity
--  fromRecordFields = identity

toRecord :: forall a rep fields. Generic a rep => GRecordFieldsIso rep () fields => a -> Record fields
toRecord a = Builder.buildFromScratch $ gToRecordFields $ from a

fromRecord :: forall a rep row fields. Generic a rep => GRecordFieldsIso rep row fields => Record fields -> a
fromRecord a = to $ gFromRecordFields a

{-
y :: { a :: Int }
y = Builder.buildFromScratch $
  toRecordFields (tagged (Identity 1) :: Tagged "a" (Identity Int))

-- type T = RL.Cons "Tuple2" (RL.Cons "a" Int (RL.Cons "b" String RL.Nil)) RL.Nil

z :: { a :: Int, b :: String }
z =
  let
    a :: Tuple2 (Tagged "a" (Identity Int)) (Tagged "b" (Identity String))
    a = Tuple2 (tagged (Identity 1) :: Tagged "a" (Identity Int)) (tagged (Identity "hell") :: Tagged "b" (Identity String))
  --b = from a :: Int

  in
    Builder.buildFromScratch $ gToRecordFields $ from a
-}
