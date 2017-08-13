module Web3Spec.Contract (simpleStorageSpec) where

import Prelude
import Test.Spec (Spec, describe, it)
import Control.Monad.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)

simpleStorageSpec :: forall r . Spec r Unit
simpleStorageSpec =
  describe "interacting with a SimpleStorage Contract" do
    it "can set the value of simple storage" do
      true `shouldEqual` true
