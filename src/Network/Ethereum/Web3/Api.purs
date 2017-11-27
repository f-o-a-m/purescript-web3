module Network.Ethereum.Web3.Api where

import Prelude

import Network.Ethereum.Web3.JsonRPC (remote)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider)
import Network.Ethereum.Web3.Types (Web3, Address, BigNumber, Block, CallMode, HexString, Transaction, TransactionOptions, Change, FilterId, Filter, SyncStatus)
import Network.Ethereum.Web3.Types.Types (unsafeCoerceWeb3, defaultTransactionOptions, FalseOrObject)

-- | Returns current node version string.
web3_clientVersion :: forall p e . IsAsyncProvider p => Web3 p e String
web3_clientVersion = unsafeCoerceWeb3 $ remote "web3_clientVersion" :: Web3 p () String

-- | Returns the balance of the account of given address.
eth_getBalance :: forall p e . IsAsyncProvider p => Address -> CallMode -> Web3 p e BigNumber
eth_getBalance addr cm = unsafeCoerceWeb3 $ remote "eth_getBalance" addr cm :: Web3 p () BigNumber

-- | Returns information about a block by number.
eth_getBlockByNumber :: forall p e . IsAsyncProvider p => CallMode -> Web3 p e Block
eth_getBlockByNumber cm = unsafeCoerceWeb3 $ remote "eth_getBlockByNumber" cm false :: Web3 p () Block

-- | Returns information about a block by hash.
eth_getBlockByHash :: forall p e . IsAsyncProvider p => HexString -> Web3 p e Block
eth_getBlockByHash hx = unsafeCoerceWeb3 $ remote "eth_getBlockByHash" hx :: Web3 p () Block

-- | Returns information about a transaction by hash.
eth_getTransaction :: forall p e . IsAsyncProvider p => HexString -> Web3 p e Transaction
eth_getTransaction hx = unsafeCoerceWeb3 $ remote "eth_getTransactionByHash" hx :: Web3 p () Transaction

-- | Call a function on a particular block's state root.
eth_call :: forall p e . IsAsyncProvider p => TransactionOptions -> CallMode -> Web3 p e HexString
eth_call opts cm = unsafeCoerceWeb3 $ remote "eth_call" opts cm :: Web3 p () HexString

-- | Creates new message call transaction or a contract creation, if the data field contains code.
eth_sendTransaction :: forall p e . IsAsyncProvider p => TransactionOptions -> Web3 p e HexString
eth_sendTransaction opts = unsafeCoerceWeb3 $ remote "eth_sendTransaction" opts :: Web3 p () HexString

-- | Get all account addresses registered at the `Provider`
eth_getAccounts :: forall p e . IsAsyncProvider p => Web3 p e (Array Address)
eth_getAccounts = unsafeCoerceWeb3 $ remote "eth_accounts" defaultTransactionOptions :: Web3 p () (Array Address)

-- | Creates a filter object, based on filter options, to notify when the
-- | state changes (logs). To check if the state has changed, call 'eth_getFilterChanges'.
eth_newFilter :: forall p e . IsAsyncProvider p => Filter -> Web3 p e FilterId
eth_newFilter f = unsafeCoerceWeb3 $ remote "eth_newFilter" f :: Web3 p () FilterId

-- | Polling method for a filter, which returns an array of logs which occurred since last poll.
eth_getFilterChanges :: forall p e . IsAsyncProvider p => FilterId -> Web3 p e (Array Change)
eth_getFilterChanges fid = unsafeCoerceWeb3 $ remote "eth_getFilterChanges" fid :: Web3 p () (Array Change)

-- | Uninstalls a filter with given id. Should always be called when watch is no longer needed.
eth_uninstallFilter :: forall p e . IsAsyncProvider p => FilterId -> Web3 p e Boolean
eth_uninstallFilter fid = unsafeCoerceWeb3 $ remote "eth_uninstallFilter" fid :: Web3 p () Boolean

-- | Get the network id that the node is listening to.
net_version :: forall p e . IsAsyncProvider p => Web3 p e BigNumber
net_version = unsafeCoerceWeb3 $ remote "net_version" :: Web3 p () BigNumber

-- | Get the sync status of the node.
eth_getSyncing :: forall p e. IsAsyncProvider p => Web3 p e (FalseOrObject SyncStatus)
eth_getSyncing = unsafeCoerceWeb3 $ remote "eth_syncing" :: Web3 p () (FalseOrObject SyncStatus)

-- | Get the number of currently connected peers.
net_getPeerCount :: forall p e. IsAsyncProvider p => Web3 p e (BigNumber)
net_getPeerCount = unsafeCoerceWeb3 $ remote "net_peerCount" :: Web3 p () (BigNumber)


