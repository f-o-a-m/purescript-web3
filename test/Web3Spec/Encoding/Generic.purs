module Web3Spec.Encoding.Generic (encodingGenericSpec) where


import Data.Record.Builder (build, merge)
import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3.Solidity (Tuple3(..), Tuple2(..))
import Network.Ethereum.Web3.Solidity.Generic (genericToRecordFields)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

encodingGenericSpec :: forall r . Spec r Unit
encodingGenericSpec = describe "encoding-spec for generics" do
  toRecordFieldsSpec


toRecordFieldsSpec :: forall r . Spec r Unit
toRecordFieldsSpec =
    describe "test ToRecordFields class" do

      it "pass toRecordFields basic test" do
        let as = Tuple3 (tagged 1) (tagged "hello") (tagged 'c') :: Tuple3 (Tagged (SProxy "a") Int) (Tagged (SProxy "d") String) (Tagged (SProxy "e") Char)
        WeirdTuple (genericToRecordFields as) `shouldEqual` WeirdTuple { a : 1
                                                                       , d : "hello"
                                                                       , e : 'c'
                                                                       }

      it "passes the merging test" do
        let as = Tuple3 (tagged 1) (tagged "hello") (tagged 'c') :: Tuple3 (Tagged (SProxy "a") Int) (Tagged (SProxy "d") String) (Tagged (SProxy "e") Char)
            as' = Tuple2 (tagged 2) (tagged "bye") :: Tuple2 (Tagged (SProxy "b") Int) (Tagged (SProxy "c") String)
            c = CombinedTuple $ build (merge (genericToRecordFields as)) (genericToRecordFields as')
        c `shouldEqual` CombinedTuple {a: 1, b: 2, c: "bye", d: "hello", e: 'c'}


newtype WeirdTuple = WeirdTuple {a :: Int, d :: String, e :: Char}

derive instance genericWeirdTuple :: Generic WeirdTuple _

instance showWeirdTuple :: Show WeirdTuple where
  show = genericShow

instance eqWeirdTuple :: Eq WeirdTuple where
  eq = genericEq

newtype OtherTuple = OtherTuple {b :: Int, c :: String}

derive instance genericOtherTuple :: Generic OtherTuple _

instance showOtherTuple :: Show OtherTuple where
  show = genericShow

instance eqOtherTuple :: Eq OtherTuple where
  eq = genericEq

data CombinedTuple = CombinedTuple {a :: Int, b :: Int, c :: String, d :: String, e :: Char}

derive instance genericCombinedTuple :: Generic CombinedTuple _

instance showCombinedTuple :: Show CombinedTuple where
  show = genericShow

instance eqCombinedTuple :: Eq CombinedTuple where
  eq = genericEq
