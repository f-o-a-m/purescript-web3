import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Array ((!!))
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (Address, ChainCursor(Latest), EventAction(TerminateEvent), _from, _fromBlock, _to, _toBlock, defaultTransactionOptions, embed, event, eventFilter, mkAddress, mkHexString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

mockERC20Spec :: forall r. TestConfig (mockERC20 :: Address | r) -> Spec Unit
mockERC20Spec {accounts, provider, mockERC20} =
  describe "interacting with a ComplexStorage Contract" $ do
    it "can set the values of simple storage" $ do
      let primaryAccount = unsafePartial $ fromJust $ accounts !! 0
      var <- AVar.empty
      let amount = unsafePartial $ fromJust <<< uIntNFromBigNumber s256 <<< embed $ 1
          to = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just mockERC20
      hx <- runWeb3 provider $ MockERC20.transfer txOptions {to : to, amount : amount}
      liftEffect $ log $ "setValues tx hash: " <> show hx

      let fltTransfer = eventFilter (Proxy :: Proxy MockERC20.Transfer) mockERC20
                          # _fromBlock .~ Latest -- (BN <<< wrap <<< embed $ 4732740)
                          # _toBlock   .~ Latest -- (BN <<< wrap <<< embed $ 4732754)

      _ <- liftAff $ runWeb3 provider $
        event fltTransfer $ \e@(MockERC20.Transfer tfr) -> do
          liftEffect $ log $ "Received transfer event: " <> show e
          liftEffect $ log $ "Value of `amount` field is: " <> show tfr.amount
          liftEffect $ log $ "Value of `from` field is: " <> show tfr.from
          _ <- liftAff $ AVar.put e var
          pure TerminateEvent
      (MockERC20.Transfer tfr) <- AVar.take var
      tfr.amount `shouldEqual` amount
