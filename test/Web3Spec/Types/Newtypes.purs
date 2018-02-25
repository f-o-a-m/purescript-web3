module Web3Spec.Types.Newtypes (ntTests) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (try)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Network.Ethereum.Web3 (ChainCursor(..), ETH, HexString, Web3, Web3Error, defaultFilter, defaultTransactionOptions, httpProvider, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api (eth_getBlockByNumber, eth_getFilterChanges, eth_getSyncing, eth_getTransaction, eth_getTransactionReceipt, eth_newFilter)
import Network.Ethereum.Web3.Types (FalseOrObject(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)

runWeb3_
    :: forall a eff
    .  Web3 eff a
    -> Aff (eth :: ETH | eff) (Either Web3Error a)
runWeb3_ action = do
    p <- liftEff $ httpProvider $ "http://localhost:8545"
    runWeb3 p action

-- note: this does not need to work, just typecheck
runNtTest :: forall a r e . Newtype a r => Web3 e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runNtTest web3req = map wrap <$> map unwrap <$> runWeb3_ web3req


fakeTxid :: HexString
fakeTxid = unsafePartial fromJust $ mkHexString "00"


-- | tests that typecheck if these types derive newtype
ntTests :: forall eff. Spec (eth :: ETH | eff) Unit
ntTests = describe "newtype-tests" do
    it "Block should derive newtype" $ do
        _ <- runNtTest $ eth_getBlockByNumber Latest
        pure unit

    it "Transaction should derive newtype" $ do
        _ <- runNtTest $ eth_getTransaction fakeTxid
        pure unit

    it "TransactionReceipt should derive newtype" $ do
        _ <- runNtTest $ eth_getTransactionReceipt fakeTxid
        pure unit

    it "TransactionOptions should derive newtype" $ do
        let _ = unwrap defaultTransactionOptions
        pure unit

    it "SyncStatus should derive newtype" $ do
        _ <- runNtTest eth_getSyncing
        pure unit

    it "Filter should derive newtype" $ do
        let _ = unwrap defaultFilter
        pure unit

    it "Change should derive newtype" $ do
        _ <- try $ runWeb3_ $ do
            fId <- eth_newFilter defaultFilter
            (map $ unwrap) <$> eth_getFilterChanges fId
        pure unit

    it "FalseOrObject should derive newtype" $ do
        let _ = unwrap $ FalseOrObject Nothing
        pure unit
