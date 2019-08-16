module Web3Spec.Live.Utils where

import Prelude

import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), Fiber, joinFiber, delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Core.Signatures (mkAddress)
import Network.Ethereum.Web3 (class EventFilter, class KnownSize, Address, Web3Error, BigNumber, BlockNumber, BytesN, CallError, DLProxy, EventAction(..), HexString, Provider, TransactionOptions, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, _from, _gas, defaultTransactionOptions, embed, event, eventFilter, forkWeb3', fromByteString, intNFromBigNumber, mkHexString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (class DecodeEvent, IntN)
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

assertStorageCall
  :: forall a.
     Provider
  -> Web3 (Either CallError a)
  -> Aff a
assertStorageCall p f = do
  eRes <- assertWeb3 p f
  case eRes of
    Right x -> pure x
    Left err -> unsafeCrashWith $ "expected Right in `assertStorageCall`, got error" <> show err

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

hangOutTillBlock :: BlockNumber -> Web3 Unit
hangOutTillBlock bn = do
  bn' <- Api.eth_blockNumber
  liftAff $ C.log $ "Current block number : " <> show bn' 
  when (bn' > bn) do
    liftAff $ delay (Milliseconds 1000.0)
    hangOutTillBlock bn 

awaitNextBlock :: Web3 Unit
awaitNextBlock = do
  n <- Api.eth_blockNumber
  let next = wrap $ embed 1 + unwrap n
  liftAff $ C.log $ "Awaiting block number " <> show next
  hangOutTillBlock next

type ContractConfig =
  { contractAddress :: Address
  , userAddress :: Address
  }

deployContract
  :: Provider
  -> String -- contract name
  -> (TransactionOptions NoPay -> Web3 HexString) -- deployment transaction
  -> Aff ContractConfig
deployContract p contractName deploymentTx = do
  userAddress <- assertWeb3 p $ do
    accounts <- Api.eth_getAccounts
    pure $ unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
  txHash <- assertWeb3 p do
    accounts <- Api.eth_getAccounts
    let txOpts = defaultTestTxOptions # _from ?~ userAddress
    txHash <- deploymentTx txOpts
    liftEffect $ C.log $ "Submitted " <> contractName <> " deployment : " <> show txHash
    pure txHash
  let k (TransactionReceipt rec) = case rec.contractAddress of
        Nothing -> unsafeCrashWith "Contract deployment missing contractAddress in receipt"
        Just addr -> pure addr
  contractAddress <- pollTransactionReceipt txHash p k
  C.log $ contractName <> " successfully deployed to " <> show contractAddress
  pure $ {contractAddress, userAddress}

joinWeb3Fork
  :: forall a.
     Fiber (Either Web3Error a)
  -> Aff a
joinWeb3Fork fiber = do
  eRes <- joinFiber fiber
  case eRes of
    Left e -> unsafeCrashWith $ "Error in forked web3 process " <> show e
    Right a -> pure a

mkHexString'
  :: String
  -> HexString
mkHexString' hx =
  unsafePartialBecause "I know how to make a HexString" $ fromJust $ mkHexString hx

mkUIntN
  :: forall n.
     KnownSize n
  => DLProxy n
  -> Int
  -> UIntN n
mkUIntN p n = unsafePartialBecause "I know how to make a UInt" $ fromJust $ uIntNFromBigNumber p $ embed n


mkIntN
  :: forall n.
     KnownSize n
  => DLProxy n
  -> Int
  -> IntN n
mkIntN p n = unsafePartialBecause "I know how to make an Int" $ fromJust $ intNFromBigNumber p $ embed n

mkBytesN
  :: forall n.
     KnownSize n
  => DLProxy n
  -> String
  -> BytesN n
mkBytesN p s = unsafePartialBecause "I know how to make Bytes" $ fromJust $ fromByteString p =<< flip BS.fromString BS.Hex s

defaultTestTxOptions :: TransactionOptions NoPay
defaultTestTxOptions =
  defaultTransactionOptions # _gas ?~ bigGasLimit

nullAddress :: Address
nullAddress = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0000000000000000000000000000000000000000"

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
