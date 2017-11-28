module Web3Spec.Encoding.Generic (encodingGenericSpec) where


import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3.Solidity (Tuple4(..))
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
        let as = Tuple4 (tagged 1) (tagged "hello") (tagged 'c') (tagged true) :: Tuple4 (Tagged (SProxy "a") Int) (Tagged (SProxy "b") String) (Tagged (SProxy "c") Char) (Tagged (SProxy "d") Boolean)
        genericToRecordFields as  `shouldEqual` WeirdTuple { a : 1
                                                           , b : "hello"
                                                           , c : 'c'
                                                           , d : true
                                                           }

data WeirdTuple = WeirdTuple {a :: Int, b :: String, c :: Char, d :: Boolean}

derive instance genericWeirdTuple :: Generic WeirdTuple _

instance showWeirdTuple :: Show WeirdTuple where
  show = genericShow

instance eqWeirdTuple :: Eq WeirdTuple where
  eq = genericEq
