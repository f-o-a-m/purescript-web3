module Web3Spec.Types.Sha3 (sha3Spec) where

import Prelude

import Network.Ethereum.Web3.Types.Sha3 (sha3)
import Network.Ethereum.Web3.Types.Types (unsafeHexString)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

sha3Spec :: forall r . Spec r Unit
sha3Spec = describe "sha3-spec" do
    describe "Sha3 tests" do
      it "can hash strings" do
        sha3 "test123" `shouldEqual` unsafePartial unsafeHexString "f81b517a242b218999ec8eec0ea6e2ddbef2a367a14e93f4a32a39e260f686ad"
        sha3 "test(int)" `shouldEqual` unsafePartial unsafeHexString "f4d03772bec1e62fbe8c5691e1a9101e520e8f8b5ca612123694632bf3cb51b1"
      it "can hash hex strings" do
        sha3 (unsafePartial unsafeHexString "80") `shouldEqual` unsafePartial unsafeHexString "56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421"
        sha3 "0x80" `shouldEqual` unsafePartial unsafeHexString "6b03a5eef7706e3fb52a61c19ab1122fad7237726601ac665bd4def888f0e4a0"
        sha3 (unsafePartial unsafeHexString "3c9229289a6125f7fdf1885a77bb12c37a8d3b4962d936f7e3084dece32a3ca1") `shouldEqual` unsafePartial unsafeHexString "82ff40c0a986c6a5cfad4ddf4c3aa6996f1a7837f9c398e17e5de5cbd5a12b28"
