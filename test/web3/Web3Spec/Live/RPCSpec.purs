module Web3Spec.Live.RPCSpec (spec) where

import Prelude
import Data.Array ((!!))
import Data.ByteString as BS
import Data.Either (isRight)
import Data.Lens ((?~), (%~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Network.Ethereum.Core.HexString as Hex
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures as Sig
import Network.Ethereum.Web3 (Block(..), ChainCursor(..), Provider, TransactionReceipt(..), _from, _to, _value, convert, defaultTransactionOptions, fromMinorUnit, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api as Api
import Node.Buffer.Class (slice)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Quotient (runQuotient)
import Web3Spec.Live.Utils (assertWeb3, pollTransactionReceipt)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec provider =
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
      eRes <-
        runWeb3 provider
          $ do
              accounts <- Api.eth_getAccounts
              Api.eth_getBalance (unsafePartial fromJust $ accounts !! 0) Latest
      eRes `shouldSatisfy` isRight
    it "Can call eth_getTransactionCount" do
      eRes <-
        runWeb3 provider do
          accounts <- Api.eth_getAccounts
          Api.eth_getTransactionCount (unsafePartial fromJust $ accounts !! 0) Latest
      eRes `shouldSatisfy` isRight
    it "Can call eth_getBlockByNumber, eth_getBlockTransactionCountByHash, getBlockTransactionCountByNumber" do
      Tuple count1 count2 <-
        assertWeb3 provider do
          bn <- Api.eth_blockNumber
          Block block <- Api.eth_getBlockByNumber (BN bn)
          let
            bHash = unsafePartial fromJust block.hash
          count1 <- Api.eth_getBlockTransactionCountByHash bHash
          count2 <- Api.eth_getBlockTransactionCountByNumber (BN bn)
          pure $ Tuple count1 count2
      count1 `shouldEqual` count2
    it "Can call eth_getUncleCountByBlockHash eth_getUncleCountByBlockNumber" do
      Tuple count1 count2 <-
        assertWeb3 provider do
          bn <- Api.eth_blockNumber
          Block block <- Api.eth_getBlockByNumber (BN bn)
          let
            bHash = unsafePartial fromJust block.hash
          count1 <- Api.eth_getUncleCountByBlockHash bHash
          count2 <- Api.eth_getUncleCountByBlockNumber (BN bn)
          pure $ Tuple count1 count2
      count1 `shouldEqual` count2
    it "Can call eth_getBlockByHash" do
      eRes <-
        runWeb3 provider do
          bn <- Api.eth_blockNumber
          Block block <- Api.eth_getBlockByNumber (BN bn)
          let
            bHash = unsafePartial fromJust block.hash
          Api.eth_getBlockByHash bHash
      eRes `shouldSatisfy` isRight
    -- TODO: validate this with eth-core lib
    it "Can call personal_sign, personal_ecRecover, and they should coincide with eth-core" do
      let
        msgBody = unsafePartial fromJust $ mkHexString "1234"

        fullHashedMessageBS = keccak256 <<< makeRidiculousEthereumMessage $ msgBody
      { signer, signer', signatureHex } <-
        assertWeb3 provider do
          accounts <- Api.eth_getAccounts
          let
            signer = unsafePartial fromJust $ accounts !! 0
          signatureHex <- Api.personal_sign msgBody signer (Just "password123")
          signer' <- Api.personal_ecRecover msgBody signatureHex
          pure $ { signer, signer', signatureHex }
      signer `shouldEqual` signer'
      -- make sure that we can recover the signature in purescript natively
      let
        rsvSignature = case signatureFromByteString <<< Hex.toByteString $ signatureHex of
          Sig.Signature sig -> Sig.Signature sig { v = sig.v - 27 }
      Sig.publicToAddress (Sig.recoverSender fullHashedMessageBS rsvSignature) `shouldEqual` signer
    it "Can call eth_estimateGas" do
      eRes <- runWeb3 provider $ Api.eth_estimateGas (defaultTransactionOptions # _value %~ map convert)
      eRes `shouldSatisfy` isRight
    it "Can call eth_getTransactionByBlockHashAndIndex eth_getBlockTransactionByBlockNumberAndIndex" do
      txHash <-
        assertWeb3 provider do
          accounts <- Api.eth_getAccounts
          let
            sender = unsafePartial fromJust $ accounts !! 0

            receiver = unsafePartial fromJust $ accounts !! 1

            txOpts =
              defaultTransactionOptions # _from ?~ sender
                # _to
                ?~ receiver
                # _value
                ?~ fromMinorUnit one
          Api.eth_sendTransaction txOpts
      TransactionReceipt txReceipt <- pollTransactionReceipt provider txHash pure
      Tuple tx tx' <-
        assertWeb3 provider do
          tx <- Api.eth_getTransactionByBlockHashAndIndex txReceipt.blockHash zero
          tx' <- Api.eth_getTransactionByBlockNumberAndIndex (BN txReceipt.blockNumber) zero
          pure $ Tuple tx tx'
      tx `shouldEqual` tx'

signatureFromByteString :: BS.ByteString -> Sig.Signature
signatureFromByteString bs =
  let
    bfr = BS.unsafeThaw bs

    r = Hex.fromByteString $ BS.unsafeFreeze $ slice 0 32 bfr

    s = Hex.fromByteString $ BS.unsafeFreeze $ slice 32 64 bfr

    v = runQuotient $ unsafePartial fromJust $ BS.last bs
  in
    Sig.Signature { r, s, v }

makeRidiculousEthereumMessage :: Hex.HexString -> Hex.HexString
makeRidiculousEthereumMessage s =
  let
    prefix =
      Hex.fromByteString
        $ BS.toUTF8
        $ "\x19" -- NOTE: 19 in hexadecimal is 25
        <> "Ethereum Signed Message:\n" -- NOTE: length of this string is 25
        <> show (Hex.numberOfBytes s)
  in
    prefix <> s
