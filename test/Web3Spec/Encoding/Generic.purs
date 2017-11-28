module Web3Spec.Encoding.Generic (encodingGenericSpec) where


import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Record.Builder (build, merge)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3.Solidity (Tuple3(..), Tuple2(..))
import Network.Ethereum.Web3.Solidity.Generic (genericToRecordFields, genericFromRecordFields)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

encodingGenericSpec :: forall r . Spec r Unit
encodingGenericSpec = describe "encoding-spec for generics" do
  toRecordFieldsSpec


toRecordFieldsSpec :: forall r . Spec r Unit
toRecordFieldsSpec =
    describe "test ToRecordFields class" do

      it "pass toRecordFields basic test" do
        let as = Tuple3 (tagged 1) (tagged "hello") (tagged 'c') :: Tuple3 (Tagged (SProxy "_1") Int) (Tagged (SProxy "_4") String) (Tagged (SProxy "_5") Char)
        genericToRecordFields as `shouldEqual` WeirdTuple { _1 : 1
                                                          , _4 : "hello"
                                                          , _5 : 'c'
                                                          }

      it "pass fromRecordFields basic test" do
        let as = Tuple2 2 "bye" :: Tuple2 Int String
        genericFromRecordFields (OtherTuple { _2 : 2, _3 : "bye"}) `shouldEqual` as

      it "passes the merging test" do
        let as = Tuple3 (tagged 1) (tagged "hello") (tagged 'c') :: Tuple3 (Tagged (SProxy "_1") Int) (Tagged (SProxy "_4") String) (Tagged (SProxy "_5") Char)
            as' = Tuple2 (tagged 2) (tagged "bye") :: Tuple2 (Tagged (SProxy "_2") Int) (Tagged (SProxy "_3") String)
            WeirdTuple fs = genericToRecordFields as
            OtherTuple fs' = genericToRecordFields as'
            c = CombinedTuple $ build (merge fs) fs'
        genericFromRecordFields c `shouldEqual` Combined 1 2 "bye" "hello" 'c'


newtype WeirdTuple = WeirdTuple {_1 :: Int, _4 :: String, _5 :: Char}

derive instance genericWeirdTuple :: Generic WeirdTuple _

instance showWeirdTuple :: Show WeirdTuple where
  show = genericShow

instance eqWeirdTuple :: Eq WeirdTuple where
  eq = genericEq

newtype OtherTuple = OtherTuple {_2 :: Int, _3 :: String}

derive instance genericOtherTuple :: Generic OtherTuple _

instance showOtherTuple :: Show OtherTuple where
  show = genericShow

instance eqOtherTuple :: Eq OtherTuple where
  eq = genericEq

data CombinedTuple = CombinedTuple {_1 :: Int, _2 :: Int, _3 :: String, _4 :: String, _5 :: Char}

derive instance genericCombinedTuple :: Generic CombinedTuple _

instance showCombinedTuple :: Show CombinedTuple where
  show = genericShow

instance eqCombinedTuple :: Eq CombinedTuple where
  eq = genericEq

data Combined = Combined Int Int String String Char

derive instance genericCombined :: Generic Combined _

instance showCombined :: Show Combined where
  show = genericShow

instance eqCombined :: Eq Combined where
  eq = genericEq
