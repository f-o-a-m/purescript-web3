module Web3Spec.Types.Newtypes (ntTests) where

import Prelude

import Control.Monad.Aff (Aff, Error)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (try)
import Data.Array (head)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Network.Ethereum.Web3 (class IsAsyncProvider, Block(..), ChainCursor(..), HexString, Web3, Web3Error, defaultFilter, defaultTransactionOptions, httpProvider, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_getBlockByNumber, eth_getFilterChanges, eth_getSyncing, eth_getTransaction, eth_getTransactionReceipt, eth_newFilter)
import Network.Ethereum.Web3.Types (FalseOrObject(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

-- Set up HTTP provider for testing, note, this doesn't
-- need to actually work, just needs to typecheck
data HttpProvider

http :: Proxy HttpProvider
http = Proxy

instance isAsyncHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = liftEff <<< httpProvider $ "http://localhost:8545"

runWeb3_ = runWeb3 http

-- note: this does not need to work, just typecheck
runNtTest :: forall a r. Newtype a _ => Web3 _ _ a -> Aff _ (Either Web3Error a)
runNtTest web3req = map wrap <$> map unwrap <$> runWeb3_ web3req


fakeTxid :: HexString
fakeTxid = unsafePartial fromJust $ mkHexString "00"


-- | tests that typecheck if these types derive newtype
ntTests :: forall r . Spec _ Unit
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
