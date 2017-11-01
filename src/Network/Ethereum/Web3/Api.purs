module Network.Ethereum.Web3.Api where

import Prelude

import Network.Ethereum.Web3.JsonRPC (remote, remoteAsync)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, class IsSyncProvider)
import Network.Ethereum.Web3.Types (Web3M, Web3MA, Address, BigNumber, Block, CallMode, HexString, Transaction, TransactionOptions, Change, FilterId, Filter)
import Network.Ethereum.Web3.Types.Types (unsafeCoerceWeb3M, unsafeCoerceWeb3MA, defaultTransactionOptions)

-- | Returns current node version string.
web3_clientVersion :: forall p e . IsSyncProvider p => Web3M p e String
web3_clientVersion = unsafeCoerceWeb3M $ remote "web3_clientVersion" :: Web3M p () String

-- | Returns the balance of the account of given address.
eth_getBalance :: forall p e . IsSyncProvider p => Address -> CallMode -> Web3M p e BigNumber
eth_getBalance addr cm = unsafeCoerceWeb3M $ remote "eth_getBalance" addr cm :: Web3M p () BigNumber

-- | Returns information about a block by hash.
-- Use the boolean flag =true if you want the whole transaction objects
-- in the 'transactions' field, =false to get just the hash
eth_getBlockByNumber :: forall p e . IsSyncProvider p => CallMode -> Web3M p e Block
eth_getBlockByNumber cm = unsafeCoerceWeb3M $ remote "eth_getBlockByNumber" cm false :: Web3M p () Block

eth_getBlockByHash :: forall p e . IsSyncProvider p => HexString -> Web3M p e Block
eth_getBlockByHash hx = unsafeCoerceWeb3M $ remote "eth_getBlockByHash" hx :: Web3M p () Block

eth_getTransaction :: forall p e . IsSyncProvider p => HexString -> Web3M p e Transaction
eth_getTransaction hx = unsafeCoerceWeb3M $ remote "eth_getTransactionByHash" hx :: Web3M p () Transaction

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
eth_call :: forall p e . IsSyncProvider p => TransactionOptions -> CallMode -> Web3M p e HexString
eth_call opts cm = unsafeCoerceWeb3M $ remote "eth_call" opts cm :: Web3M p () HexString

eth_call_async :: forall p e . IsAsyncProvider p => TransactionOptions -> CallMode -> Web3MA p e HexString
eth_call_async opts cm = unsafeCoerceWeb3MA $ remoteAsync "eth_call" opts cm :: Web3MA p () HexString

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
eth_sendTransaction :: forall p e . IsSyncProvider p => TransactionOptions -> Web3M p e HexString
eth_sendTransaction opts = unsafeCoerceWeb3M $ remote "eth_sendTransaction" opts :: Web3M p () HexString

eth_sendTransaction_async :: forall p e . IsAsyncProvider p => TransactionOptions -> Web3MA p e HexString
eth_sendTransaction_async opts = unsafeCoerceWeb3MA $ remoteAsync "eth_sendTransaction" opts :: Web3MA p () HexString

eth_getAccounts :: forall p e . IsAsyncProvider p => Web3MA p e (Array Address)
eth_getAccounts = unsafeCoerceWeb3MA $ remoteAsync "eth_accounts" defaultTransactionOptions :: Web3MA p () (Array Address)

-- | Creates a filter object, based on filter options, to notify when the
-- state changes (logs). To check if the state has changed, call
-- 'getFilterChanges'.
eth_newFilter :: forall p e . IsAsyncProvider p => Filter -> Web3MA p e FilterId
eth_newFilter f = unsafeCoerceWeb3MA $ remoteAsync "eth_newFilter" f :: Web3MA p () FilterId

-- | Polling method for a filter, which returns an array of logs which
-- occurred since last poll.
eth_getFilterChanges :: forall p e . IsAsyncProvider p => FilterId -> Web3MA p e (Array Change)
eth_getFilterChanges fid = unsafeCoerceWeb3MA $ remoteAsync "eth_getFilterChanges" fid :: Web3MA p () (Array Change)

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
eth_uninstallFilter :: forall p e . IsAsyncProvider p => FilterId -> Web3MA p e Boolean
eth_uninstallFilter fid = unsafeCoerceWeb3MA $ remoteAsync "eth_uninstallFilter" fid :: Web3MA p () Boolean

-- | Uninstalls a filter with given id.
-- Should always be called when watch is no longer needed.
net_version :: forall p e . IsAsyncProvider p => Web3MA p e BigNumber
net_version = unsafeCoerceWeb3MA $ remoteAsync "net_version" :: Web3MA p () BigNumber
