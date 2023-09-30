module Web3Spec.Encoding.GenericSpec (spec) where

import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic, from)
import Data.Identity (Identity(..))
import Effect.Class (liftEffect)
import Network.Ethereum.Web3.Solidity (Tuple2(..), Tuple3(..))
import Network.Ethereum.Web3.Solidity.Internal (toRecord)
import Record (disjointUnion)
import Record.Builder (build, merge)
import Test.QuickCheck (quickCheck, (===))
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "encoding-spec for generics" do
    toRecordFieldsSpec

{-

    Constructor "Tuple2" 
      (Product 
        (Argument (Tagged "as" T)) (Argument (Tagged "bs" R)))

    Constructor "Tuple2" (Product (Argument (Tagged "a" Int)) (Argument (Tagged "b" Int)))

-}

toRecordFieldsSpec :: Spec Unit
toRecordFieldsSpec =
  describe "test ToRecordFields class" do
    it "pass toRecordFields basic test" $ liftEffect do
      quickCheck $ \(x :: { a :: Int, b :: Int, c :: String, d :: String }) ->
        let
          as = Tuple2 (tagged $ Identity x.a) (tagged $ Identity x.b) :: Tuple2 (Tagged "a" (Identity Int)) (Tagged "b" (Identity Int))
          bs = Tuple2 (tagged $ Identity x.c) (tagged $ Identity x.d) :: Tuple2 (Tagged "c" (Identity String)) (Tagged "d" (Identity String))
          cs = Tuple2 (tagged as) (tagged bs)
        --q = from as :: Int
        in
          -- disjointUnion (toRecord as) (toRecord bs)
          toRecord cs
            ===
              { as: { a: x.a, b: x.b }
              , bs: { c: x.c, d: x.d }
              }

    it "pass toRecordFields basic test" $ liftEffect do
      quickCheck $ \(x :: { a :: Int, b :: Int, c :: String, d :: String, e :: Char }) ->
        let
          as = Tuple3 (tagged x.a) (tagged x.d) (tagged x.e) :: Tuple3 (Tagged "a" Int) (Tagged "d" String) (Tagged "e" Char)
        in
          WeirdTuple (toRecord as)
            ===
              WeirdTuple
                { a: x.a
                , d: x.d
                , e: x.e
                }

    it "passes the merging test" $ liftEffect do
      quickCheck $ \(x :: { a :: Int, b :: Int, c :: String, d :: String, e :: Char }) ->
        let
          as = Tuple3 (tagged x.a) (tagged x.d) (tagged x.e) :: Tuple3 (Tagged "a" Int) (Tagged "d" String) (Tagged "e" Char)

          as' = Tuple2 (tagged x.b) (tagged x.c) :: Tuple2 (Tagged "b" Int) (Tagged "c" String)

          c = CombinedTuple $ build (merge (toRecord as)) (toRecord as')
        in
          c === CombinedTuple x

--------------------------------------------------------------------------------

newtype WeirdTuple = WeirdTuple { a :: Int, d :: String, e :: Char }

derive instance Generic WeirdTuple _
derive newtype instance Show WeirdTuple
derive newtype instance Eq WeirdTuple

newtype OtherTuple = OtherTuple { b :: Int, c :: String }

derive instance Generic OtherTuple _
derive newtype instance Show OtherTuple
derive newtype instance Eq OtherTuple

newtype CombinedTuple = CombinedTuple { a :: Int, b :: Int, c :: String, d :: String, e :: Char }

derive instance Generic CombinedTuple _
derive newtype instance Show CombinedTuple
derive newtype instance Eq CombinedTuple
