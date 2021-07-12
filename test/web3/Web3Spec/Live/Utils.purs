module Web3Spec.Live.Utils where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (intercalate)
import Data.Array.NonEmpty as NAE
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), Fiber, joinFiber, delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as C
import Test.Spec (ComputationType(..), SpecT, hoistSpec)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Web3 (class EventFilter, class KnownSize, Address, Web3Error, BigNumber, BlockNumber, BytesN, CallError, DLProxy, EventAction(..), HexString, Provider, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, _from, _gas, defaultTransactionOptions, event, embed, eventFilter, forkWeb3', fromByteString, intNFromBigNumber, mkHexString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (class DecodeEvent, IntN)
import Network.Ethereum.Web3.Types (NoPay)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Type.Proxy (Proxy)

type Logger m
  = String -> m Unit

go :: SpecT (ReaderT (Logger Aff) Aff) Unit Aff ~> SpecT Aff Unit Aff
go =
  hoistSpec identity \cType m ->
    let
      prefix = case cType of
        CleanUpWithContext n -> intercalate " > " n <> " (afterAll) "
        TestWithName n -> intercalate " > " $ NAE.toArray n
    in
      runReaderT m \logMsg -> C.log $ prefix <> "| " <> logMsg

-- | Run a `Web3` action which will dispatch a single event, wait for the event,
-- | then return the action's result and the event.
takeEvent ::
  forall a ev i ni.
  DecodeEvent i ni ev =>
  Show ev =>
  EventFilter ev =>
  Proxy ev ->
  Address ->
  Web3 a ->
  Web3 (Tuple a ev)
takeEvent prx addrs web3Action = do
  var <- liftAff AVar.empty
  _ <-
    forkWeb3' do
      event (eventFilter prx addrs)
        $ \e -> do
            _ <- liftAff $ AVar.put e var
            pure TerminateEvent
  efRes <- web3Action
  event <- liftAff $ AVar.take var
  pure $ Tuple efRes event

-- | Assert the `Web3` action's result, crash the program if it doesn't succeed.
assertWeb3 ::
  forall m a.
  MonadAff m =>
  Provider ->
  Web3 a ->
  m a
assertWeb3 provider a =
  liftAff $ runWeb3 provider a
    <#> case _ of
        Right x -> x
        Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

assertStorageCall ::
  forall m a.
  MonadAff m =>
  Provider ->
  Web3 (Either CallError a) ->
  m a
assertStorageCall p f =
  liftAff do
    eRes <- assertWeb3 p f
    case eRes of
      Right x -> pure x
      Left err -> unsafeCrashWith $ "expected Right in `assertStorageCall`, got error" <> show err

pollTransactionReceipt ::
  forall m a.
  MonadAff m =>
  Provider ->
  HexString ->
  (TransactionReceipt -> Aff a) ->
  m a
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

hangOutTillBlock ::
  forall m.
  MonadAff m =>
  Provider ->
  Logger m ->
  BlockNumber ->
  m Unit
hangOutTillBlock provider logger bn = do
  bn' <- assertWeb3 provider Api.eth_blockNumber
  logger $ "Current block number : " <> show bn'
  when (bn' < bn) do
    liftAff $ delay (Milliseconds 1000.0)
    hangOutTillBlock provider logger bn

awaitNextBlock ::
  forall m.
  MonadAff m =>
  Provider ->
  Logger m ->
  m Unit
awaitNextBlock provider logger = do
  n <- assertWeb3 provider Api.eth_blockNumber
  let
    next = wrap $ embed 1 + unwrap n
  logger $ "Awaiting block number " <> show next
  hangOutTillBlock provider logger next

type ContractConfig
  = { contractAddress :: Address
    , userAddress :: Address
    }

deployContract ::
  forall m.
  MonadAff m =>
  Provider ->
  Logger m ->
  String ->
  (TransactionOptions NoPay -> Web3 HexString) ->
  m ContractConfig
deployContract p logger contractName deploymentTx = do
  userAddress <-
    assertWeb3 p
      $ do
          accounts <- Api.eth_getAccounts
          pure $ unsafePartial fromJust $ accounts !! 0
  txHash <-
    assertWeb3 p do
      let
        txOpts = defaultTestTxOptions # _from ?~ userAddress
      txHash <- deploymentTx txOpts
      pure txHash
  logger $ "Submitted " <> contractName <> " deployment : " <> show txHash
  let
    k (TransactionReceipt rec) = case rec.contractAddress of
      Nothing -> unsafeCrashWith "Contract deployment missing contractAddress in receipt"
      Just addr -> pure addr
  contractAddress <- pollTransactionReceipt p txHash k
  logger $ contractName <> " successfully deployed to " <> show contractAddress
  pure $ { contractAddress, userAddress }

joinWeb3Fork ::
  forall a m.
  MonadAff m =>
  Fiber (Either Web3Error a) ->
  m a
joinWeb3Fork fiber =
  liftAff do
    eRes <- joinFiber fiber
    case eRes of
      Left e -> unsafeCrashWith $ "Error in forked web3 process " <> show e
      Right a -> pure a

mkHexString' ::
  String ->
  HexString
mkHexString' hx = unsafePartial fromJust $ mkHexString hx

mkUIntN ::
  forall n.
  KnownSize n =>
  DLProxy n ->
  Int ->
  UIntN n
mkUIntN p n = unsafePartial fromJust $ uIntNFromBigNumber p $ embed n

mkIntN ::
  forall n.
  KnownSize n =>
  DLProxy n ->
  Int ->
  IntN n
mkIntN p n = unsafePartial fromJust $ intNFromBigNumber p $ embed n

mkBytesN ::
  forall n.
  KnownSize n =>
  DLProxy n ->
  String ->
  BytesN n
mkBytesN p s = unsafePartial fromJust $ fromByteString p =<< flip BS.fromString BS.Hex s

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions = defaultTransactionOptions # _gas ?~ bigGasLimit

nullAddress :: Address
nullAddress = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
