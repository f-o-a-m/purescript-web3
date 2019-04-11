module Web3Spec.Live.LiveSpec where

import Prelude

import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (Either(..), isRight, fromRight)
import Data.Lens ((?~), (%~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal, BigNumber)
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures as Sig
import Network.Ethereum.Web3 (Block(..), ChainCursor(..), EventAction(..), HexString, Provider, TransactionReceipt(..), TransactionStatus(..), Web3, _from, _gas, _to, _value, convert, defaultTransactionOptions, event, eventFilter, forkWeb3, fromMinorUnit, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Node.Buffer.Unsafe (slice)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))
import Type.Quotient (mkQuotient, runQuotient)
import Web3Spec.Live.SimpleStorage as SimpleStorage

liveSpec :: Provider -> Spec Unit
liveSpec provider =
  describe "It should be able to test all the web3 endpoints live" do

    it "Can get the network version" do
      eRes <- runWeb3 provider $ Api.net_version
      eRes `shouldSatisfy` isRight

    it "Can call net_listening" do
      eRes <- runWeb3 provider $ Api.net_listening
      eRes `shouldSatisfy` isRight

    it "Can call net_getPeerCount" do
      eRes <- runWeb3 provider $ Api.net_getPeerCount
      eRes `shouldSatisfy` isRight

    it "Can call eth_protocolVersion" do
      eRes <- runWeb3 provider $ Api.eth_protocolVersion
      eRes `shouldSatisfy` isRight

    it "Can call eth_getSyncing" do
      eRes <- runWeb3 provider $ Api.eth_getSyncing
      eRes `shouldSatisfy` isRight

    it "Can call eth_coinbase" do
      eRes <- runWeb3 provider $ Api.eth_coinbase
      eRes `shouldSatisfy` isRight

    it "Can call eth_mining" do
      eRes <- runWeb3 provider $ Api.eth_mining
      eRes `shouldSatisfy` isRight

    it "Can call eth_hashrate" do
      eRes <- runWeb3 provider $ Api.eth_hashrate
      eRes `shouldSatisfy` isRight

    it "Can call eth_blockNumber" do
      eRes <- runWeb3 provider $ Api.eth_blockNumber
      eRes `shouldSatisfy` isRight

    it "Can call eth_accounts and eth_getBalance" do
      eRes <- runWeb3 provider $ do
        accounts <- Api.eth_getAccounts
        Api.eth_getBalance (unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0) Latest
      eRes `shouldSatisfy` isRight

    it "Can call eth_getTransactionCount" do
      eRes <- runWeb3 provider do
        accounts <- Api.eth_getAccounts
        Api.eth_getTransactionCount (unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0) Latest
      eRes `shouldSatisfy` isRight

    it "Can call eth_getBlockByNumber, eth_getBlockTransactionCountByHash, getBlockTransactionCountByNumber" do
      eRes <- runWeb3 provider do
        bn <- Api.eth_blockNumber
        Block block <- Api.eth_getBlockByNumber (BN bn)
        let bHash = unsafePartialBecause "Block is not pending" $ fromJust block.hash
        count1 <- Api.eth_getBlockTransactionCountByHash bHash
        count2 <- Api.eth_getBlockTransactionCountByNumber (BN bn)
        pure $ Tuple count1 count2
      eRes `shouldSatisfy` isRight
      let Tuple count1 count2 = unsafePartialBecause "Result was Right" $ fromRight eRes
      count1 `shouldEqual` count2

    it "Can call eth_getUncleCountByBlockHash eth_getUncleCountByBlockNumber" do
      eRes <- runWeb3 provider do
        bn <- Api.eth_blockNumber
        Block block <- Api.eth_getBlockByNumber (BN bn)
        let bHash = unsafePartialBecause "Block is not pending" $ fromJust block.hash
        count1 <- Api.eth_getUncleCountByBlockHash bHash
        count2 <- Api.eth_getUncleCountByBlockNumber (BN bn)
        pure $ Tuple count1 count2
      eRes `shouldSatisfy` isRight
      let Tuple count1 count2 = unsafePartialBecause "Result was Right" $ fromRight eRes
      count1 `shouldEqual` count2

    it "Can call eth_getBlockByHash" do
      eRes <- runWeb3 provider do
        bn <- Api.eth_blockNumber
        Block block <- Api.eth_getBlockByNumber (BN bn)
        let bHash = unsafePartialBecause "Block is not pending" $ fromJust block.hash
        Api.eth_getBlockByHash bHash
      eRes `shouldSatisfy` isRight

    -- TODO: validate this with eth-core lib
    it "Can call personal_sign, personal_ecRecover, and they should coincide with eth-core" do
      let msgBody = unsafePartial fromJust $ mkHexString "1234"
          fullHashedMessageBS = keccak256 <<< makeRidiculousEthereumMessage $ msgBody
      eRes <- runWeb3 provider do
        accounts <- Api.eth_getAccounts
        let signer = unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
        signatureHex <- Api.personal_sign msgBody signer (Just "password123")
        signer' <- Api.personal_ecRecover msgBody signatureHex
        pure $ {signer, signer', signatureHex}
      eRes `shouldSatisfy` isRight
      let {signer, signer', signatureHex} = unsafePartialBecause "Result was Right" $ fromRight eRes
      signer `shouldEqual` signer'
      -- make sure that we can recover the signature in purescript natively
      let rsvSignature = case signatureFromByteString <<< Hex.toByteString $ signatureHex of
                           Sig.Signature sig -> Sig.Signature sig {v = sig.v - 27}
      Sig.publicToAddress (Sig.recoverSender fullHashedMessageBS rsvSignature) `shouldEqual` signer

    it "Can call eth_estimateGas" do
      eRes <- runWeb3 provider $ Api.eth_estimateGas (defaultTransactionOptions # _value %~ map convert)
      eRes `shouldSatisfy` isRight

    it "Can call eth_getTransactionByBlockHashAndIndex eth_getBlockTransactionByBlockNumberAndIndex" do
      eRes <- runWeb3 provider do
        accounts <- Api.eth_getAccounts
        let sender = unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
            receiver = unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 1
            txOpts = defaultTransactionOptions # _from ?~ sender
                                               # _to ?~ receiver
                                               # _value ?~ fromMinorUnit one
        Api.eth_sendTransaction txOpts
      eRes `shouldSatisfy` isRight
      let txHash = unsafePartialBecause "Result was Right" $ fromRight eRes
      TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
      eRes' <- runWeb3 provider do
        tx <- Api.eth_getTransactionByBlockHashAndIndex txReceipt.blockHash zero
        tx' <- Api.eth_getTransactionByBlockNumberAndIndex (BN txReceipt.blockNumber) zero
        pure $ Tuple tx tx'
      eRes' `shouldSatisfy` isRight
      let Tuple tx tx' = unsafePartialBecause "Result was Right" $ fromRight eRes'
      tx `shouldEqual` tx'

    it "Can deploy a contract, verify the contract storage, make a transaction, get get the event, make a call" do
      let newCount = unsafePartialBecause "one is a UINT" $ fromJust (uIntNFromBigNumber s256 one)
      eventVar <-AVar.empty
      eRes <- runWeb3 provider deploySimpleStorage
      eRes `shouldSatisfy` isRight
      let txHash = unsafePartialBecause "Result was Right" $ fromRight eRes
      (TransactionReceipt txReceipt) <- pollTransactionReceipt txHash provider
      txReceipt.status `shouldEqual` Succeeded
      let simpleStorageAddress = unsafePartialBecause "Contract deployment succeded" $ fromJust txReceipt.contractAddress
          fltr = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
      _ <- forkWeb3 provider $ event fltr \(SimpleStorage.CountSet {_count}) -> liftAff do
        liftEffect $ C.log $ "New Count Set: " <> show _count
        AVar.put _count eventVar
        pure TerminateEvent
      let countSetOptions = defaultTransactionOptions
      _ <- runWeb3 provider do
        accounts <- Api.eth_getAccounts
        let sender = unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
            txOpts = defaultTransactionOptions # _from ?~ sender
                                               # _to ?~ simpleStorageAddress
                                               # _gas ?~ bigGasLimit
        setCountHash <- SimpleStorage.setCount txOpts {_count: newCount}
        liftEffect $ C.log $ "Sumbitted count update transaction: " <> show setCountHash
      n <- AVar.take eventVar
      n `shouldEqual` newCount
      eRes' <- runWeb3 provider $ Api.eth_getStorageAt simpleStorageAddress zero Latest
      eRes' `shouldSatisfy` isRight



--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------

shouldSatisfy
  :: forall a.
     Show a
  => Eq a
  => a
  -> (a -> Boolean)
  -> Aff Unit
shouldSatisfy a p =
  if p a then pure unit else fail $ "Predicate failed: " <> show a

mkHexString'
  :: String
  -> HexString
mkHexString' hx =
  unsafePartialBecause "I know how to make a HexString" $ fromJust $ mkHexString hx

bigGasLimit :: BigNumber
bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"


pollTransactionReceipt
  :: HexString
  -> Provider
  -> Aff TransactionReceipt
pollTransactionReceipt txHash provider = do
  eRes <- runWeb3 provider $ Api.eth_getTransactionReceipt txHash
  case eRes of
    Left e -> do
      delay (Milliseconds 2000.0)
      pollTransactionReceipt txHash provider
    Right res -> pure res

deploySimpleStorage :: Web3 HexString
deploySimpleStorage = do
  accounts <- Api.eth_getAccounts
  let sender = unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
      txOpts = defaultTransactionOptions # _from ?~ sender
                                         # _gas ?~ bigGasLimit
  txHash <- SimpleStorage.constructor txOpts SimpleStorage.deployBytecode
  liftEffect $ C.log $ "Submitted SimpleStorage deployment: " <> show txHash
  pure txHash

signatureToByteString :: Sig.Signature -> BS.ByteString
signatureToByteString (Sig.Signature sig) =
  Hex.toByteString sig.r <> Hex.toByteString sig.s <> BS.singleton (mkQuotient sig.v)

signatureFromByteString :: BS.ByteString -> Sig.Signature
signatureFromByteString bs =
  let bfr = BS.unsafeThaw bs
      r = Hex.fromByteString $ BS.unsafeFreeze $ slice 0 32 bfr
      s = Hex.fromByteString $ BS.unsafeFreeze $ slice 32 64 bfr
      v = runQuotient $ unsafePartial fromJust $ BS.last bs
  in Sig.Signature {r,s,v}

makeRidiculousEthereumMessage :: Hex.HexString -> Hex.HexString
makeRidiculousEthereumMessage s =
  let prefix = Hex.fromByteString <<< BS.toUTF8 $ "\EMEthereum Signed Message:\n" <> show (Hex.hexLength s `div` 2)
  in prefix <> s
