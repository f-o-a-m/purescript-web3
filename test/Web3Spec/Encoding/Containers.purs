module Web3Spec.Encoding.Containers where --(encodingSpec) where


--import Prelude
--import Data.Maybe (Maybe(..))
--import Test.Spec (Spec, describe, it)
--import Control.Monad.Aff (Aff)
--import Test.Spec.Assertions (shouldEqual)
--import Network.Ethereum.Web3.Types
--import Network.Ethereum.Web3.Encoding (class ABIEncoding, toDataBuilder, fromData)
--import Network.Ethereum.Web3.Encoding.Internal ()
--
--
--encodingSpec :: forall r . Spec r Unit
--encodingSpec = describe "encoding-spec" do
--  stringTests
--  bytesDTests
--  bytesNTests
--  intTests
--  addressTests
--
--roundTrip :: forall r a . Show a => Eq a => ABIEncoding a => a -> HexString -> Aff r Unit
--roundTrip decoded encoded = do
--  encoded `shouldEqual` toDataBuilder decoded
--  fromData encoded `shouldEqual` Just decoded
--
--stringTests :: forall r . Spec r Unit
--stringTests =
--    describe "dynamic array tests" do
--
--      it "can encode dynamic arrays of ints" do
--         let given = "gavofyork"
--         let expected =  HexString $ "0000000000000000000000000000000000000000000000000000000000000009"
--                                  <> "6761766f66796f726b0000000000000000000000000000000000000000000000"
--         roundTrip given expected

