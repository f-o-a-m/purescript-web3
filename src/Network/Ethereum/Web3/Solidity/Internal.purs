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
import Data.Symbol (class IsSymbol, reflectSymbol)
import Debug (trace)
import Prim.Row as Row
import Record (disjointUnion)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class GRecordFieldsIso rep from to | rep -> to, to rep -> from where
  gToRecordFields :: rep -> Builder { | from } { | to }
  gFromRecordFields :: Record to -> rep

instance GRecordFieldsIso NoArguments from from where
  gToRecordFields _ = trace "no arguments" \_ ->
    identity
  gFromRecordFields _ = trace "no arguments" \_ ->
    NoArguments

else instance (IsSymbol name, GRecordFieldsIso a from to, Show (Record to), Show a) => GRecordFieldsIso (Constructor name a) from to where
  gToRecordFields (Constructor a) = trace ("constructor " <> reflectSymbol (Proxy @name) <> ":\n" <> show a) \_ ->
    gToRecordFields a
  gFromRecordFields r = trace ("constructor " <> reflectSymbol (Proxy @name) <> ":\n" <> show r) \_ ->
    Constructor (gFromRecordFields r)

else instance
  ( GRecordFieldsIso a () ato
  , GRecordFieldsIso b () bto
  , Row.Union ato bto to
  , Row.Union to from to
  , Row.Nub to to
  , Show (Record to)
  , Show a
  , Show b
  ) =>
  GRecordFieldsIso (Product a b) from to where
  gToRecordFields (Product as bs) = trace ("product:\n" <> show as <> "\n" <> show bs) $ \_ ->
    let
      r = Builder.buildFromScratch (gToRecordFields as) `disjointUnion` Builder.buildFromScratch (gToRecordFields bs)
    in
      Builder.merge r

  gFromRecordFields r = trace ("product:\n" <> show r) $ \_ ->
    let
      as = gFromRecordFields (unsafeCoerce r)
      bs = gFromRecordFields (unsafeCoerce r)
    in
      Product as bs

else instance
  ( ToRecordFields a from to
  , Show (Record to)
  , Show a
  ) =>
  GRecordFieldsIso (Argument a) from to where
  gToRecordFields (Argument a) = trace ("argument:\n" <> show a) \_ ->
    toRecordFields a
  gFromRecordFields r = trace ("argument:\n" <> show r) \_ ->
    Argument $ fromRecordFields r

class ToRecordFields a from to | from a -> to, a to -> from where
  toRecordFields :: a -> Builder { | from } { | to }
  fromRecordFields :: Record to -> a

instance
  ( IsSymbol s
  , Row.Cons s (Array (Record to)) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  , Show (Record to')
  , Show a
  ) =>
  ToRecordFields (Tagged s (Array a)) from to' where
  toRecordFields a = trace ("array:\n" <> show a) \_ ->
    Builder.insert (Proxy @s) $ map
      (Builder.buildFromScratch <<< (gToRecordFields <<< from))
      (untagged a)
  fromRecordFields r =
    trace ("array:\n" <> show r) \_ ->
      tagged $ map (to <<< gFromRecordFields) $ Record.get (Proxy @s) r

else instance foo ::
  ( IsSymbol s
  , Row.Cons s a from to
  , Row.Lacks s from
  , Show (Record to)
  , Show a
  ) =>
  ToRecordFields (Tagged s (Identity a)) from to where
  toRecordFields a = trace ("identity:\n" <> show a) \_ ->
    Builder.insert (Proxy @s) (un Identity $ untagged a)
  fromRecordFields r = trace ("identity:\n" <> show r) \_ ->
    tagged $ Identity $ Record.get (Proxy @s) r

else instance bar ::
  ( IsSymbol s
  , Row.Cons s (Record to) from to'
  , Row.Lacks s from
  , Generic a rep
  , GRecordFieldsIso rep () to
  , Show (Record to')
  , Show a
  ) =>
  ToRecordFields (Tagged s a) from to' where
  toRecordFields a = trace ("tagged " <> reflectSymbol (Proxy @s) <> ":\n" <> show a) \_ ->
    Builder.insert (Proxy @s)
      $ Builder.buildFromScratch (gToRecordFields $ from $ untagged a)
  fromRecordFields r = trace ("tagged " <> reflectSymbol (Proxy @s) <> ":\n" <> show r) \_ ->
    tagged $ to $ gFromRecordFields $ Record.get (Proxy @s) r

--else instance ToRecordFields (Record r) r r where
--  toRecordFields _ = identity
--  fromRecordFields = identity

toRecord :: forall a rep fields. Generic a rep => Show a => GRecordFieldsIso rep () fields => a -> Record fields
toRecord a = trace ("toRecord:\n" <> show a) \_ ->
  Builder.buildFromScratch $ gToRecordFields $ from a

fromRecord :: forall a rep row fields. Generic a rep => Show a => Show (Record fields) => GRecordFieldsIso rep row fields => Record fields -> a
fromRecord a = trace ("fromRecord:\n" <> show a) \_ ->
  to $ gFromRecordFields a

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
