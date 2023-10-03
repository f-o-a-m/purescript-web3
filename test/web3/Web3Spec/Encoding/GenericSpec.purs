module Web3Spec.Encoding.GenericSpec (spec) where

import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Effect.Class (liftEffect)
import Network.Ethereum.Web3.Solidity (Tuple2(..), Tuple3(..))
import Network.Ethereum.Web3.Solidity.Internal (toRecord)
import Record.Builder (build, merge)
import Test.QuickCheck (quickCheck, (===))
import Test.Spec (Spec, describe, it)

spec :: Spec Unit
spec =
  describe "encoding-spec for generics" do
    toRecordFieldsSpec

toRecordFieldsSpec :: Spec Unit
toRecordFieldsSpec =
  describe "test ToRecordFields class" do
    it "pass toRecordFields basic test" $ liftEffect do
      quickCheck $ \(x :: { a :: Int, b :: Int, c :: String, d :: String }) ->
        let
          as = Tuple2 (tagged $ Identity x.a) (tagged $ Identity x.b) :: Tuple2 (Tagged "a" (Identity Int)) (Tagged "b" (Identity Int))
          bs = Tuple2 (tagged $ Identity x.c) (tagged $ Identity x.d) :: Tuple2 (Tagged "c" (Identity String)) (Tagged "d" (Identity String))
          cs = Tuple2 (tagged as :: Tagged "as" _) (tagged bs :: Tagged "bs" _)
        --q = from as :: Int
        in
          toRecord cs
            ===
              { as: { a: x.a, b: x.b }
              , bs: { c: x.c, d: x.d }
              }

    it "pass toRecordFields basic test" $ liftEffect do
      quickCheck $ \(x :: { a :: Int, b :: Int, c :: String, d :: String, e :: Char }) ->
        let
          as = Tuple3 (tagged $ Identity x.a) (tagged $ Identity x.d) (tagged $ Identity x.e) :: Tuple3 (Tagged "a" (Identity Int)) (Tagged "d" (Identity String)) (Tagged "e" (Identity Char))
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
          as = Tuple3 (tagged $ Identity x.a) (tagged $ Identity x.d) (tagged $ Identity x.e) :: Tuple3 (Tagged "a" (Identity Int)) (Tagged "d" (Identity String)) (Tagged "e" (Identity Char))

          as' = Tuple2 (tagged $ Identity x.b) (tagged $ Identity x.c) :: Tuple2 (Tagged "b" (Identity Int)) (Tagged "c" (Identity String))

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
