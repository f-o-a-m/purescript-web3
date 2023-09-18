module Web3Spec.Types.EtherUnitSpec (spec) where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Ring.Module (class LeftModule, mzeroL, (^*), (^+), (^-))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Network.Ethereum.Core.BigNumber (pow)
import Network.Ethereum.Web3 (Shannon, Szabo, Value, Wei, Ether, _value, convert, defaultTransactionOptions, formatValue, fromInt, fromMinorUnit, mkValue)
import Network.Ethereum.Web3.Types.TokenUnit as Value
import Test.QuickCheck (arbitrary, quickCheck', (===))
import Test.QuickCheck.Gen (Gen, chooseInt)
import Test.QuickCheck.Laws (checkLaws)
import Test.QuickCheck.Laws.Data as Data
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "ether unit spec" do
  describe "conversion tests" do
    it "can encode convert from a higher denomination to lower" do
      let
        inEth = convert (mkValue one :: Value Ether)

        inWei = (mkValue $ (fromInt 10) `pow` 18) :: Value Wei
      inEth `shouldEqual` inWei
      let
        shannon = mkValue (fromInt 10 `pow` 3) :: Value Shannon

        szabo = mkValue one :: Value Szabo
      convert shannon `shouldEqual` szabo

    it "can perform arithmetic" do
      let
        two = mkValue (fromInt 1 + fromInt 1) :: Value Shannon

        two' = mkValue one ^+ mkValue one
      two `shouldEqual` two'
      (two ^- two') `shouldEqual` mzeroL
      (2 ^* two') `shouldEqual` mkValue (fromInt 4)

    it "can use the lens properly" do
      let
        noPay = defaultTransactionOptions

        opts = defaultTransactionOptions # _value .~ Just (convert (mkValue one :: Value Ether))
      (noPay ^. _value) `shouldEqual` Nothing
      (opts ^. _value) `shouldEqual` (Just (fromMinorUnit (fromInt 10 `pow` 18) :: Value Wei))

    it "can format currencies correctly" do
      let
        n = mkValue (fromInt 1) :: Value Ether

        m = convert n :: Value Wei

        -- making the loop shouldn't change the result
        n' = convert m :: Value Ether
      formatValue n `shouldEqual` "1"
      formatValue n' `shouldEqual` "1"
      formatValue m `shouldEqual` "1000000000000000000"

  describe "laws" do
    it "satisfies basic laws" $ liftEffect $ checkLaws "Value Ether" $ do
      Data.checkEqGen $ Value.generator (Proxy @Ether)
      Data.checkOrdGen $ Value.generator (Proxy @Ether)
      Data.checkSemigroupGen $ Value.generator (Proxy @Ether)
      Data.checkMonoidGen $ Value.generator (Proxy @Ether)
      checkLeftModuleGen arbitrary smallIntsGen (Value.generator (Proxy @Ether))

checkLeftModuleGen
  :: forall r m
   . LeftModule m r
  => Eq m
  => Show m
  => Gen r
  -- need this because of overflow (see smallIntsGen)
  -> Gen (Tuple r r)
  -> Gen m
  -> Effect Unit
checkLeftModuleGen genR genRR genM = do
  log "Checking 'Distributivity1' law for LeftModule"
  quickCheck' 1000 $ lift3 distributivity1 genR genM genM
  log "Checking 'Distributivity2' law for LeftModule"
  quickCheck' 1000 $ lift3 distributivity2 genR genR genM
  log "Checking 'Compatibility' law for LeftModule"
  quickCheck' 1000 $ lift2 compatibility genRR genM
  log "Checking 'identity' law for LeftModule"
  quickCheck' 1000 $ (_identity <$> genM)

  where
  distributivity1 r x y = r ^* (x ^+ y) === r ^* x ^+ r ^* y
  distributivity2 r s x = (r + s) ^* x === r ^* x ^+ s ^* x
  compatibility (Tuple r s) x = (r * s) ^* x === r ^* (s ^* x)
  _identity x = one ^* x === x

smallIntsGen :: Gen (Tuple Int Int)
smallIntsGen = do
  n1 <- arbitrary
  n2 <- chooseInt 0 (top `div` n1)
  pure $ Tuple n1 n2