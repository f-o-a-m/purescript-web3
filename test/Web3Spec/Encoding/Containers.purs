module Web3Spec.Encoding.Containers (encodingContainersSpec) where


import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Test.Spec (Spec, describe, it)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)
import Data.ByteString as BS
import Network.Ethereum.Web3.Types (Address(..), HexString(..))
import Network.Ethereum.Web3.Encoding.Vector (Vector, toVector)
import Network.Ethereum.Web3.Encoding.Bytes(BytesN, fromByteString)
import Network.Ethereum.Web3.Encoding.Size (N1, N2, N4, D1)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder, fromData)


encodingContainersSpec :: forall r . Spec r Unit
encodingContainersSpec = describe "encoding-spec" do
  staticArraysTests

roundTrip :: forall r a . Show a => Eq a => ABIEncoding a => a -> HexString -> Aff r Unit
roundTrip decoded encoded = do
  encoded `shouldEqual` toDataBuilder decoded
  fromData encoded `shouldEqual` Just decoded

staticArraysTests :: forall r . Spec r Unit
staticArraysTests =
    describe "statically sized array tests" do

      it "can encode statically sized vectors of addresses" do
         let mgivenElement = toVector $ [false]
             givenElement = (unsafePartial $ fromJust $ mgivenElement) :: Vector N1 Boolean
             given = (unsafePartial $ fromJust $ toVector [givenElement, givenElement]) :: Vector N2 (Vector N1 Boolean)
             expected =  HexString $ "0000000000000000000000000000000000000000000000000000000000000000"
                                  <> "0000000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

      it "can encode statically sized vectors of statically sized vectors of type bool" do
         let mgiven = toVector $ map (Address <<< HexString) [ "407d73d8a49eeb85d32cf465507dd71d507100c1"
                                                             , "407d73d8a49eeb85d32cf465507dd71d507100c3"
                                                             ]
             given = (unsafePartial $ fromJust $ mgiven) :: Vector N2 Address
             expected =  HexString $ "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c1"
                                  <> "000000000000000000000000407d73d8a49eeb85d32cf465507dd71d507100c3"
         roundTrip given expected

      it "can encode statically sized vectors of statically sized bytes" do
         let elem1 = (unsafePartial $ fromJust <<< fromByteString <<< flip BS.fromString BS.Hex $ "cf") :: BytesN D1
             elem2 = (unsafePartial $ fromJust <<< fromByteString <<< flip BS.fromString BS.Hex $ "68") :: BytesN D1
             elem3 = (unsafePartial $ fromJust <<< fromByteString <<< flip BS.fromString BS.Hex $ "4d") :: BytesN D1
             elem4 = (unsafePartial $ fromJust <<< fromByteString <<< flip BS.fromString BS.Hex $ "fb") :: BytesN D1
             given = (unsafePartial $ fromJust <<< toVector $ [elem1, elem2, elem3, elem4]) :: Vector N4 (BytesN D1)
             expected =  HexString $ "cf00000000000000000000000000000000000000000000000000000000000000"
                                  <> "6800000000000000000000000000000000000000000000000000000000000000"
                                  <> "4d00000000000000000000000000000000000000000000000000000000000000"
                                  <> "fb00000000000000000000000000000000000000000000000000000000000000"
         roundTrip given expected

