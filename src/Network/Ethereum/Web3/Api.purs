module Network.Ethereum.Web3.Api where

import Prelude
import Network.Ethereum.Web3.Types (Web3M, Web3MA, Address, BigNumber, Block, CallMode, HexString, Transaction, TransactionOptions, unsafeCoerceWeb3M, unsafeCoerceWeb3MA)
import Network.Ethereum.Web3.JsonRPC (remote, remoteAsync)


-- | Returns current node version string.
web3_clientVersion :: forall e . Web3M e String
web3_clientVersion = unsafeCoerceWeb3M $ remote "web3_clientVersion" :: Web3M () String

-- | Returns the balance of the account of given address.
eth_getBalance :: forall e . Address -> CallMode -> Web3M e BigNumber
eth_getBalance addr cm = unsafeCoerceWeb3M $ remote "eth_getBalance" addr cm :: Web3M () BigNumber

-- | Returns information about a block by hash.
-- Use the boolean flag =true if you want the whole transaction objects
-- in the 'transactions' field, =false to get just the hash
eth_getBlockByNumber :: forall e . CallMode -> Web3M e Block
eth_getBlockByNumber cm = unsafeCoerceWeb3M $ remote "eth_getBlockByNumber" cm false :: Web3M () Block

eth_getBlockByHash :: forall e . HexString -> Web3M e Block
eth_getBlockByHash hx = unsafeCoerceWeb3M $ remote "eth_getBlockByHash" hx :: Web3M () Block

eth_getTransaction :: forall e . HexString -> Web3M e Transaction
eth_getTransaction hx = unsafeCoerceWeb3M $ remote "eth_getTransactionByHash" hx :: Web3M () Transaction

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
eth_call :: forall e . TransactionOptions -> CallMode -> Web3M e HexString
eth_call opts cm = unsafeCoerceWeb3M $ remote "eth_call" opts cm :: Web3M () HexString

eth_call_async :: forall e . TransactionOptions -> CallMode -> Web3MA e HexString
eth_call_async opts cm = unsafeCoerceWeb3MA $ remoteAsync "eth_call" opts cm :: Web3MA () HexString

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
eth_sendTransaction :: forall e . TransactionOptions -> Web3M e HexString
eth_sendTransaction opts = unsafeCoerceWeb3M $ remote "eth_sendTransaction" opts :: Web3M () HexString

eth_sendTransaction_async :: forall e . TransactionOptions -> Web3MA e HexString
eth_sendTransaction_async opts = unsafeCoerceWeb3MA $ remoteAsync "eth_sendTransaction" opts :: Web3MA () HexString
