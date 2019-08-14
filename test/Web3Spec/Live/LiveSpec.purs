module Web3Spec.Live.LiveSpec (liveSpec) where

import Prelude

import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (isRight, fromRight)
import Data.Lens ((?~), (%~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures as Sig
import Network.Ethereum.Web3 (Address, Block(..), ChainCursor(..), EventAction(..), Provider, TransactionReceipt(..), _from, _to, _value, convert, defaultTransactionOptions, event, eventFilter, forkWeb3, fromMinorUnit, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Node.Buffer.Unsafe (slice)
import Partial.Unsafe (unsafeCrashWith, unsafePartial, unsafePartialBecause)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Type.Quotient (mkQuotient, runQuotient)
import Web3Spec.Live.SimpleStorage as SimpleStorage
import Web3Spec.LiveSpec.Utils (assertWeb3, defaultTestTxOptions, pollTransactionReceipt)

liveSpec :: Provider -> SpecT Aff Unit Aff Unit
liveSpec p = do
  rpcSpec p
  contractSpec p

rpcSpec :: Provider -> SpecT Aff Unit Aff Unit
rpcSpec provider =
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
      TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider pure
      eRes' <- runWeb3 provider do
        tx <- Api.eth_getTransactionByBlockHashAndIndex txReceipt.blockHash zero
        tx' <- Api.eth_getTransactionByBlockNumberAndIndex (BN txReceipt.blockNumber) zero
        pure $ Tuple tx tx'
      eRes' `shouldSatisfy` isRight
      let Tuple tx tx' = unsafePartialBecause "Result was Right" $ fromRight eRes'
      tx `shouldEqual` tx'


contractSpec :: Provider -> SpecT Aff Unit Aff Unit
contractSpec provider = beforeAll (deploySimpleStorage provider) $
  describe "It should be able to deploy and test a simple contract" $

    it "Can deploy a contract, verify the contract storage, make a transaction, get get the event, make a call" $ \simpleStorageCfg -> do
      let {simpleStorageAddress, userAddress} = simpleStorageCfg
          newCount = unsafePartialBecause "one is a UINT" $ fromJust (uIntNFromBigNumber s256 one)
          fltr = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
      eventVar <-AVar.empty
      _ <- forkWeb3 provider $ event fltr \(SimpleStorage.CountSet {_count}) -> liftAff do
        liftEffect $ C.log $ "New Count Set: " <> show _count
        AVar.put _count eventVar
        pure TerminateEvent
      _ <- runWeb3 provider do
        let txOpts = defaultTestTxOptions # _from ?~ userAddress
                                          # _to ?~ simpleStorageAddress
        setCountHash <- SimpleStorage.setCount txOpts {_count: newCount}
        liftEffect $ C.log $ "Sumbitted count update transaction: " <> show setCountHash
      n <- AVar.take eventVar
      n `shouldEqual` newCount
      eRes' <- runWeb3 provider $ Api.eth_getStorageAt simpleStorageAddress zero Latest
      eRes' `shouldSatisfy` isRight

--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------

type SimpleStorageConfig =
  { simpleStorageAddress :: Address
  , userAddress :: Address
  }

deploySimpleStorage :: Provider -> Aff SimpleStorageConfig
deploySimpleStorage p = do
  userAddress <- assertWeb3 p $ do
    accounts <- Api.eth_getAccounts
    pure $ unsafePartialBecause "there is more than one account" $ fromJust $ accounts !! 0
  txHash <- assertWeb3 p do
    accounts <- Api.eth_getAccounts
    let txOpts = defaultTestTxOptions # _from ?~ userAddress
    txHash <- SimpleStorage.constructor txOpts SimpleStorage.deployBytecode
    liftEffect $ C.log $ "Submitted SimpleStorage deployment: " <> show txHash
    pure txHash
  let k (TransactionReceipt rec) = case rec.contractAddress of
        Nothing -> unsafeCrashWith "Contract deployment missing contractAddress in receipt"
        Just addr -> pure addr
  simpleStorageAddress <- pollTransactionReceipt txHash p k
  pure $ {simpleStorageAddress, userAddress}

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
