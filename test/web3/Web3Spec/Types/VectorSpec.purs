module Web3Spec.Types.VectorSpec (spec) where

import Prelude
import Data.Array (uncons)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (Vector, nilVector, (:<))
import Network.Ethereum.Web3.Solidity.Size (class Inc, type (:%), type (:&), D0, D1, D2, D3, D4, D8, D9, DOne)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec =
  describe "Vector-spec" do
    it "spec module compiles" $ pure unit

unsafeVector :: forall a. Vector a Int
unsafeVector = unsafeCoerce []

vec098 :: Vector (D0 :& D9 :% D8) Int -- actually not possible, b.c. `string[0]` would throw `TypeError: Array with zero length specified`
vec098 = unsafeVector

vec990 :: Vector (D9 :& D9 :% D0) Int
vec990 = unsafeVector

vec999 :: Vector (D9 :& D9 :% D9) Int
vec999 = unsafeVector

vec99999999999999 :: Vector (D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :& D9 :% D9) Int
vec99999999999999 = unsafeVector

vec9 :: Vector (DOne D9) Int
vec9 = unsafeVector

vec2 :: Vector (DOne D2) Int
vec2 = unsafeVector

test1 :: Vector (D9 :& D9 :% D1) Int
-- test1 :: Vector _ Int
test1 = 1 :< vec990 -- same as to append `1` to array with 990 elements `[x, y, ...]` - you will get array `[1, x, y, ...]`

test2 :: Vector (D1 :& D0 :& D0 :% D0) Int
-- test2 :: Vector _ Int
test2 = 1 :< vec999

test3 :: Vector (D1 :% D0) Int
-- test3 :: Vector _ Int
test3 = 1 :< vec9

test4 :: Vector (DOne D3) Int
-- test4 :: Vector _ Int
test4 = 1 :< vec2

test5 :: Vector (D1 :& D0 :& D0 :% D4) Int
-- test5 :: Vector _ Int
test5 = 1 :< 1 :< 1 :< 1 :< 1 :< vec999

test6 :: Vector (D0 :& D9 :% D9) Int
-- test6 :: Vector _ Int
test6 = 1 :< vec098

test7 :: Vector (D1 :& D0 :% D0) Int
-- test7 :: Vector _ Int
test7 = 1 :< 1 :< vec098

test8 :: Vector (D1 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :& D0 :% D1) Int
-- test8 :: Vector _ Int
test8 = 1 :< 1 :< vec99999999999999

test10 :: forall n. Inc n (D1 :% D0) => Vector n Int -> Vector (D1 :% D0) Int
test10 l = 2 :< l

-- test10_ :: Vector _ Int
test10_ :: Vector (D1 :% D0) Int
test10_ = test10 vec9

test11 :: forall n. Inc n (DOne D0) => Vector n Int -> Vector (DOne D0) Int
test11 l = 2 :< l

-- As expected `test11` can be written, but can't be called
-- test11_ = test11 nilVector
-- we can write uncons like this, but when it's used see `test12` if you
-- remove type annotation code will fail to compile. if inc and all
-- classes which it's using had reverse functional dependencies
-- then compiler could potentially infer type, but we don't have
-- such implementation for `Inc` and even with such version [1]
-- compiler still gives horrible error
-- https://gist.github.com/safareli/e1d3805a48a0a772d72ed895945c3607#file-digitswithsupperclass-purs-L38-L103
vUncons :: forall a n nDec. Inc nDec n => Vector n a -> { head :: a, tail :: Vector nDec a }
vUncons as = case uncons $ unsafeCoerce as of
  Nothing -> unsafeCrashWith "impossible case in vUncons from Network.Ethereum.Web3.Solidity.Vector"
  Just { head, tail } -> { head, tail: unsafeCoerce tail }

test12 :: Vector (DOne D3) Int
test12 = (vUncons (1 :< 1 :< 1 :< 1 :< nilVector)).tail
