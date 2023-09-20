module Web3Spec.Live.Utils
  ( assertWeb3
  , bigGasLimit
  , defaultTestTxOptions
  , go
  , pollTransactionReceipt
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array.NonEmpty as NAE
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Traversable (intercalate)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (decimal, fromStringAs)
import Network.Ethereum.Web3 (BigNumber, HexString, Provider, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), Web3, _gas, defaultTransactionOptions, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Spec (ComputationType(..), SpecT, hoistSpec)

type Logger m = String -> m Unit

go :: SpecT (ReaderT (Logger Aff) Aff) Unit Aff ~> SpecT Aff Unit Aff
go =
  hoistSpec identity \cType m ->
    let
      prefix = case cType of
        CleanUpWithContext n -> intercalate " > " n <> " (afterAll) "
        TestWithName n -> intercalate " > " $ NAE.toArray n
    in
      runReaderT m \logMsg -> C.log $ prefix <> "| " <> logMsg

-- | Assert the `Web3` action's result, crash the program if it doesn't succeed.
assertWeb3
  :: forall m a
   . MonadAff m
  => Provider
  -> Web3 a
  -> m a
assertWeb3 provider a =
  liftAff $ runWeb3 provider a
    <#> case _ of
      Right x -> x
      Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

pollTransactionReceipt
  :: forall m a
   . MonadAff m
  => Provider
  -> HexString
  -> (TransactionReceipt -> Aff a)
  -> m a
pollTransactionReceipt provider txHash k =
  liftAff do
    eRes <- runWeb3 provider $ Api.eth_getTransactionReceipt txHash
    case eRes of
      Left _ -> do
        delay (Milliseconds 2000.0)
        pollTransactionReceipt provider txHash k
      Right receipt@(TransactionReceipt res) -> case res.status of
        Succeeded -> k receipt
        Failed -> unsafeCrashWith $ "Transaction failed : " <> show txHash

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions = defaultTransactionOptions # _gas ?~ bigGasLimit

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ fromStringAs decimal "4712388"
