module Web3Spec.Encoding.GenericSpec (spec) where

import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3.Solidity (Tuple2(..), Tuple3(..))
import Network.Ethereum.Web3.Solidity.Internal (genericToRecordFields)
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
          as = Tuple2 (tagged x.a) (tagged x.b) :: Tuple2 (Tagged "a" Int) (Tagged "b" Int)
          bs = Tuple2 (tagged x.c) (tagged x.d) :: Tuple2 (Tagged "c" String) (Tagged "d" String)
        in
          (build (merge (genericToRecordFields as)) (genericToRecordFields bs))
            ===
              { a: x.a
              , b: x.b
              , c: x.c
              , d: x.d
              }

    it "pass toRecordFields basic test" $ liftEffect do
      quickCheck $ \(x :: { a :: Int, b :: Int, c :: String, d :: String, e :: Char }) ->
        let
          as = Tuple3 (tagged x.a) (tagged x.d) (tagged x.e) :: Tuple3 (Tagged "a" Int) (Tagged "d" String) (Tagged "e" Char)
        in
          WeirdTuple (genericToRecordFields as)
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

          c = CombinedTuple $ build (merge (genericToRecordFields as)) (genericToRecordFields as')
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
