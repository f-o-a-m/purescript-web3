module Web3Spec.Encoding.Generic (encodingGenericSpec) where


import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Record.Builder (build, merge)
import Data.Symbol (SProxy(..))
import Debug.Trace (traceA)
import Network.Ethereum.Web3.Solidity (class ABIDecode, fromData)
import Network.Ethereum.Web3.Solidity (type (:&), Address, D2, D5, D6, Tuple1(..), Tuple2(..), Tuple3(..), UIntN)
import Network.Ethereum.Web3.Solidity.Event (class DecodeEvent, decodeEvent)
import Network.Ethereum.Web3.Solidity.Generic (class ArrayParser, genericToRecordFields, arrayParser)
import Network.Ethereum.Web3.Types (Change(..), HexString, mkAddress, mkHexString)
import Partial.Unsafe (unsafePartial)
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


      it "can parse a change event" do
        decodeEvent change `shouldEqual` Just transfer

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

--------------------------------------------------------------------------------

type TransferIndexed = Tuple2 (Tagged (SProxy "to") Address) (Tagged (SProxy "from") Address)

type TransferNonIndexed = Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& D6)))

newtype Transfer = Transfer {to :: Address, from :: Address, amount :: UIntN (D2 :& D5 :& D6)}

instance newtypeTransfer :: Newtype Transfer (Record (to :: Address, from :: Address, amount ::UIntN (D2 :& D5 :& D6) )) where
  wrap = Transfer
  unwrap (Transfer t) = t

derive instance genericTransfer :: Generic Transfer _

instance decodeTransfer :: DecodeEvent (Tuple2 (Tagged (SProxy "to") Address) (Tagged (SProxy "from") Address)) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& D6)))) Transfer

instance showTranfer :: Show Transfer where
  show = genericShow

instance eqTransfer :: Eq Transfer where
  eq = genericEq

transfer :: Transfer
transfer =
  let t = unsafePartial fromJust $ mkAddress =<< mkHexString "407d73d8a49eeb85d32cf465507dd71d507100c1"
      f = unsafePartial fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"
      a = unsafePartial fromJust $ fromData =<< mkHexString "0000000000000000000000000000000000000000000000000000000000000001"
  in Transfer { to: t
              , from: f
              , amount: a
              }

addressArray :: Array HexString
addressArray =
  let to = unsafePartial fromJust $ mkHexString "407d73d8a49eeb85d32cf465507dd71d507100c1"
      from = unsafePartial fromJust $ mkHexString "0000000000000000000000000000000000000000"
  in [to, from]

amount :: HexString
amount = unsafePartial fromJust $ mkHexString "0000000000000000000000000000000000000000000000000000000000000001"

change :: Change
change = Change { data: amount
                , topics: addressArray
                , logIndex: li
                , transactionHash: tx
                , transactionIndex: txi
                , blockNumber: bn
                , blockHash: bh
                , address: a
                }
  where
    li = unsafePartial fromJust $ mkHexString "0"
    bh = unsafePartial fromJust $ mkHexString "0"
    tx = unsafePartial fromJust $ mkHexString "0"
    txi = unsafePartial fromJust $ mkHexString "0"
    bn = unsafePartial fromJust $ mkHexString "0"
    a = unsafePartial fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"
