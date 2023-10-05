module Web3Spec.Encoding.GenericSpec (spec) where

import Prelude

import Data.Either (Either, isRight)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Maybe (fromJust)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Web3.Solidity (BytesN, Tuple2(..), Tuple3(..), UIntN, fromRecord, toRecord)
import Network.Ethereum.Web3.Solidity.AbiEncoding (abiDecode, abiEncode)
import Network.Ethereum.Web3.Solidity.Internal (toRecord)
import Parsing (ParseError)
import Partial.Unsafe (unsafePartial)
import Record.Builder (build, merge)
import Test.QuickCheck (quickCheck, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldSatisfy)

spec :: Spec Unit
spec =
  describe "encoding-spec for generics" do
    toRecordFieldsSpec

toRecordFieldsSpec :: Spec Unit
toRecordFieldsSpec =
  describe "test ToRecordFields class" do

    it "Can parse nested tuples: " $
      let
        eRes :: Either ParseError Nested
        eRes = abiDecode nestedTupleBytes
      in
        (toRecord <$> eRes) `shouldSatisfy` isRight

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

type NestedRec =
  { x :: { a1 :: UIntN 256, a2 :: String }
  , y :: { b1 :: Array String, b2 :: BytesN 32 }
  , z ::
      Array { a :: { a1 :: UIntN 256, a2 :: String }, b :: { b1 :: Array String, b2 :: BytesN 32 } }
  }

type Nested = Tuple3
  (Tagged "x" (Tuple2 (Tagged "a1" (Identity (UIntN 256))) (Tagged "a2" (Identity String))))
  ( Tagged "y"
      (Tuple2 (Tagged "b1" (Identity (Array String))) (Tagged "b2" (Identity (BytesN 32))))
  )
  ( Tagged "z"
      ( Array
          ( Tuple2
              ( Tagged "a"
                  (Tuple2 (Tagged "a1" (Identity (UIntN 256))) (Tagged "a2" (Identity String)))
              )
              ( Tagged "b"
                  (Tuple2 (Tagged "b1" (Identity (Array String))) (Tagged "b2" (Identity (BytesN 32))))
              )
          )
      )
  )

nestedTupleBytes :: HexString
nestedTupleBytes =
  unsafePartial
    $ fromJust
    $ mkHexString "000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000e0000000000000000000000000000000000000000000000000000000000000038000000000000000000000000000000000000000000000000000badab5ed11c7ca0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000fe0a183e6bc96e0b098e9ba96e4a3b5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000406d0eb6eb8c2f86ddc21333dd73ea2ed919be82ebd61aee27be6beefce602f7c5000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000018000000000000000000000000000000000000000000000000000000000000001c00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000001aeebd9601eba6ace3af98e79297e98183eaa7acf394a5b6e285bf000000000000000000000000000000000000000000000000000000000000000000000000001fe691a0e993baefaabae194b8e7a0a5eb80b6e580b2e8898af0aab68eefbc8700000000000000000000000000000000000000000000000000000000000000000ce4b896e7ae94e79c98e8908900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003e5928000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009e59390e89b86e991ad00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012e5aba1eaa0bde7b090e6a4b3ef849ce1a68a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000badab5ed11c7ca0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000fe0a183e6bc96e0b098e9ba96e4a3b5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000406d0eb6eb8c2f86ddc21333dd73ea2ed919be82ebd61aee27be6beefce602f7c5000000000000000000000000000000000000000000000000000000000000000600000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000018000000000000000000000000000000000000000000000000000000000000001c00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000001aeebd9601eba6ace3af98e79297e98183eaa7acf394a5b6e285bf000000000000000000000000000000000000000000000000000000000000000000000000001fe691a0e993baefaabae194b8e7a0a5eb80b6e580b2e8898af0aab68eefbc8700000000000000000000000000000000000000000000000000000000000000000ce4b896e7ae94e79c98e8908900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003e5928000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009e59390e89b86e991ad00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000012e5aba1eaa0bde7b090e6a4b3ef849ce1a68a0000000000000000000000000000"
