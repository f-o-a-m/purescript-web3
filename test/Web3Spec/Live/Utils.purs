module Web3Spec.LiveSpec.Utils
  ( takeEvent
  , assertWeb3
  , pollTransactionReceipt
  , mkHexString'
  , defaultTestTxOptions
  , bigGasLimit
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (class EventFilter, Address, BigNumber, EventAction(..), HexString, Provider, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), Web3, _gas, defaultTransactionOptions, event, eventFilter, forkWeb3', mkHexString, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (class DecodeEvent)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial, unsafePartialBecause)
import Type.Proxy (Proxy)

-- | Run a `Web3` action which will dispatch a single event, wait for the event,
-- | then return the action's result and the event.
takeEvent
  :: forall a ev i ni.
     DecodeEvent i ni ev
  => Show ev
  => EventFilter ev
  => Proxy ev
  -> Address
  -> Web3 a
  -> Web3 (Tuple a ev)
takeEvent prx addrs web3Action = do
  var <- liftAff AVar.empty
  _ <- forkWeb3' do
    event (eventFilter prx addrs) $ \e -> do
      _ <- liftAff $ AVar.put e var
      pure TerminateEvent
  efRes <- web3Action
  event <- liftAff $ AVar.take var
  pure $ Tuple efRes event

-- | Assert the `Web3` action's result, crash the program if it doesn't succeed.
assertWeb3
  :: forall a.
     Provider
  -> Web3 a
  -> Aff a
assertWeb3 provider a = runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

pollTransactionReceipt
  :: forall a.
     HexString
  -> Provider
  -> (TransactionReceipt -> Aff a)
  -> Aff a
pollTransactionReceipt txHash provider k = do
  eRes <- runWeb3 provider $ Api.eth_getTransactionReceipt txHash
  case eRes of
    Left _ -> do
      delay (Milliseconds 2000.0)
      pollTransactionReceipt txHash provider k
    Right receipt@(TransactionReceipt res) -> case res.status of
      Succeeded -> k receipt
      Failed -> unsafeCrashWith $ "Transaction failed : " <> show txHash

mkHexString'
  :: String
  -> HexString
mkHexString' hx =
  unsafePartialBecause "I know how to make a HexString" $ fromJust $ mkHexString hx

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions =
  defaultTransactionOptions # _gas ?~ bigGasLimit

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
