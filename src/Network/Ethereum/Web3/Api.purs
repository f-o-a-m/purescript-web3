module Network.Ethereum.Web3.Api where

import Prelude

import Network.Ethereum.Web3.JsonRPC (remote)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider)
import Network.Ethereum.Web3.Types (Address, BigNumber, Block, BlockNumber, ChainCursor, Change, Filter, FilterId, HexString, SyncStatus, Transaction, TransactionOptions, TransactionReceipt, Web3)
import Network.Ethereum.Web3.Types.Types (unsafeCoerceWeb3, defaultTransactionOptions, FalseOrObject)
import Type.Data.Boolean (kind Boolean)

-- | Returns current node version string.
web3_clientVersion :: forall p e . IsAsyncProvider p => Web3 p e String
web3_clientVersion = unsafeCoerceWeb3 $ remote "web3_clientVersion" :: Web3 p () String

-- | Returns Keccak-256 (*not* the standardized SHA3-256) of the given data.
web3_sha3 :: forall p e. IsAsyncProvider p => HexString -> Web3 p e HexString
web3_sha3 hexInput = unsafeCoerceWeb3 $ remote "web3_sha3" hexInput :: Web3 p () HexString

-- | Get the network id that the node is listening to.
net_version :: forall p e . IsAsyncProvider p => Web3 p e BigNumber
net_version = unsafeCoerceWeb3 $ remote "net_version" :: Web3 p () BigNumber

-- | Returns `true`` if client is actively listening for network connections
net_listening :: forall p e . IsAsyncProvider p => Web3 p e Boolean
net_listening = unsafeCoerceWeb3 $ remote "net_listening" :: Web3 p () Boolean

-- | Get the number of currently connected peers.
net_getPeerCount :: forall p e. IsAsyncProvider p => Web3 p e BigNumber
net_getPeerCount = unsafeCoerceWeb3 $ remote "net_peerCount" :: Web3 p () (BigNumber)

-- | Returns the current ethereum protocol version
eth_protocolVersion :: forall p e. IsAsyncProvider p => Web3 p e String
eth_protocolVersion = unsafeCoerceWeb3 $ remote "eth_protocolVersion" :: Web3 p () String

-- | Get the sync status of the node.
eth_getSyncing :: forall p e. IsAsyncProvider p => Web3 p e (FalseOrObject SyncStatus)
eth_getSyncing = unsafeCoerceWeb3 $ remote "eth_syncing" :: Web3 p () (FalseOrObject SyncStatus)

-- | Returns the client coinbase address
eth_coinbase :: forall p e. IsAsyncProvider p => Web3 p e Address
eth_coinbase = unsafeCoerceWeb3 $ remote "eth_coinbase" :: Web3 p () Address

-- | Returns `true` if client is actively mining new blocks
eth_mining :: forall p e. IsAsyncProvider p => Web3 p e Boolean
eth_mining = unsafeCoerceWeb3 $ remote "eth_mining" :: Web3 p () Boolean

-- | Returns the number of hashes per second that the node is mining with
eth_hashrate :: forall p e. IsAsyncProvider p => Web3 p e BigNumber
eth_hashrate = unsafeCoerceWeb3 $ remote "eth_hashrate" :: Web3 p () BigNumber

-- | Returns the current price per gas in wei
eth_gasPrice :: forall p e. IsAsyncProvider p => Web3 p e BigNumber
eth_gasPrice = unsafeCoerceWeb3 $ remote "eth_gasPrice" :: Web3 p () BigNumber

-- | Returns the number of most recent block
eth_blockNumber :: forall p e. IsAsyncProvider p => Web3 p e BlockNumber
eth_blockNumber = unsafeCoerceWeb3 $ remote "eth_blockNumber" :: Web3 p () BlockNumber

-- | Returns the balance of the account of given address.
eth_getBalance :: forall p e . IsAsyncProvider p => Address -> ChainCursor -> Web3 p e BigNumber
eth_getBalance addr cm = unsafeCoerceWeb3 $ remote "eth_getBalance" addr cm :: Web3 p () BigNumber

-- | Returns the value from a storage position at a given address
eth_getStorageAt :: forall p e. IsAsyncProvider p => Address -> BigNumber -> ChainCursor -> Web3 p e HexString
eth_getStorageAt addr storagePos cm = unsafeCoerceWeb3 $ remote "eth_getStorageAt" addr storagePos cm :: Web3 p () HexString

-- | Returns the number of transactions *sent* from an address
eth_getTransactionCount :: forall p e. IsAsyncProvider p => Address -> ChainCursor -> Web3 p e BigNumber
eth_getTransactionCount addr cm = unsafeCoerceWeb3 $ remote "eth_getTransactionCount" addr cm :: Web3 p () BigNumber

-- | Returns the number of transactions in a block from a block matching the given block hash
eth_getBlockTransactionCountByHash :: forall p e. IsAsyncProvider p => HexString -> Web3 p e BigNumber
eth_getBlockTransactionCountByHash blockHash = unsafeCoerceWeb3 $ remote "eth_getBlockTransactionCountByHash" blockHash :: Web3 p () BigNumber

-- | Returns the number of transactions in a block matching the given block number
eth_getBlockTransactionCountByNumber :: forall p e. IsAsyncProvider p => ChainCursor -> Web3 p e BigNumber
eth_getBlockTransactionCountByNumber cm = unsafeCoerceWeb3 $ remote "eth_getBlockTransactionCountByNumber" cm :: Web3 p () BigNumber

-- TODO - is it appropriate for these to be Ints?

-- | Returns the number of uncles in a block from a block matching the given block hash
eth_getUncleCountByBlockHash :: forall p e. IsAsyncProvider p => HexString -> Web3 p e BigNumber
eth_getUncleCountByBlockHash blockNumber = unsafeCoerceWeb3 $ remote "eth_getUncleCountByBlockHash" blockNumber :: Web3 p () BigNumber

-- | Returns the number of uncles in a block from a block matching the given block number
eth_getUncleCountByBlockNumber :: forall p e. IsAsyncProvider p => ChainCursor -> Web3 p e BigNumber
eth_getUncleCountByBlockNumber cm = unsafeCoerceWeb3 $ remote "eth_getUncleCountByBlockNumber" cm :: Web3 p () BigNumber

-- | Returns code at a given address
eth_getCode :: forall p e. IsAsyncProvider p => Address -> ChainCursor -> Web3 p e HexString
eth_getCode addr cm = unsafeCoerceWeb3 $ remote "eth_getCode" addr cm :: Web3 p () HexString

-- | The sign method calculates an Ethereum specific signature with: `sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message)))`.
-- | By adding a prefix to the message makes the calculated signature recognisable as an Ethereum specific signature. This prevents misuse where a malicious DApp can sign arbitrary data (e.g. transaction) and use the signature to impersonate the victim.
-- | **Note** the address to sign with must be unlocked.
eth_sign :: forall p e. Warn "eth_sign is depricated in favor of personal_sign" => IsAsyncProvider p => Address -> HexString -> Web3 p e HexString
eth_sign addr msg = unsafeCoerceWeb3 $ remote "eth_sign" addr msg :: Web3 p () HexString

-- | Creates new message call transaction or a contract creation for signed transactions
eth_sendRawTransaction :: forall p e. IsAsyncProvider p => HexString -> Web3 p e HexString
eth_sendRawTransaction rawTx = unsafeCoerceWeb3 $ remote "eth_sendRawTransaction" rawTx :: Web3 p () HexString

-- | Makes a call or transaction, which won't be added to the blockchain and returns the used gas, which can be used for estimating the used gas.
eth_estimateGas :: forall p e. IsAsyncProvider p => TransactionOptions -> ChainCursor -> Web3 p e BigNumber
eth_estimateGas txOpts cm = unsafeCoerceWeb3 $ remote "eth_estimateGas" txOpts cm :: Web3 p () BigNumber

-- | Returns information about a transaction by block hash and transaction index position.
eth_getTransactionByBlockHashAndIndex :: forall p e. IsAsyncProvider p => HexString -> BigNumber -> Web3 p e Transaction
eth_getTransactionByBlockHashAndIndex blockHash txIndex = unsafeCoerceWeb3 $ remote "eth_getTransactionByBlockHashAndIndex" blockHash txIndex :: Web3 p () Transaction

-- | Returns information about a transaction by block number and transaction index position.
eth_getTransactionByBlockNumberAndIndex :: forall p e. IsAsyncProvider p => ChainCursor -> BigNumber -> Web3 p e Transaction
eth_getTransactionByBlockNumberAndIndex cm txIndex = unsafeCoerceWeb3 $ remote "eth_getTransactionByBlockNumberAndIndex" cm txIndex :: Web3 p () Transaction

-- | Returns the receipt of a transaction by transaction hash.
eth_getTransactionReceipt :: forall p e. IsAsyncProvider p => HexString -> Web3 p e TransactionReceipt
eth_getTransactionReceipt txHash = unsafeCoerceWeb3 $ remote "eth_getTransactionReceipt" txHash :: Web3 p () TransactionReceipt

-- | Returns information about a uncle of a block by hash and uncle index position.
eth_getUncleByBlockHashAndIndex :: forall p e. IsAsyncProvider p => HexString -> BigNumber -> Web3 p e Block
eth_getUncleByBlockHashAndIndex blockHash uncleIndex = unsafeCoerceWeb3 $ remote "eth_getUncleByBlockHashAndIndex" blockHash uncleIndex :: Web3 p () Block

-- | Returns information about a uncle of a block by number and uncle index position.
eth_getUncleByBlockNumberAndIndex :: forall p e. IsAsyncProvider p => ChainCursor -> BigNumber -> Web3 p e Block
eth_getUncleByBlockNumberAndIndex cm uncleIndex = unsafeCoerceWeb3 $ remote "eth_getUncleByBlockNumberAndIndex" cm uncleIndex :: Web3 p () Block

-- | Returns a list of available compilers in the client.
eth_getCompilers :: forall p e. IsAsyncProvider p => Web3 p e (Array String)
eth_getCompilers = unsafeCoerceWeb3 $ remote "eth_getCompilers" :: Web3 p () (Array String)

-- TODO: As the ABI is returned decoding this isn't trivial - not going to implement without a need
-- -- | Returns compiled solidity code.
-- eth_compileSolidity :: forall p e. IsAsyncProvider p => String -> Web3 p e HexString
-- eth_compileSolidity code = unsafeCoerceWeb3 $ remote "eth_compileSolidity" :: Web3 p () HexString

-- | Returns information about a block by number.
eth_getBlockByNumber :: forall p e . IsAsyncProvider p => ChainCursor -> Web3 p e Block
eth_getBlockByNumber cm = unsafeCoerceWeb3 $ remote "eth_getBlockByNumber" cm false :: Web3 p () Block

-- | Returns information about a block by hash.
eth_getBlockByHash :: forall p e . IsAsyncProvider p => HexString -> Web3 p e Block
eth_getBlockByHash hx = unsafeCoerceWeb3 $ remote "eth_getBlockByHash" hx :: Web3 p () Block

-- | Returns information about a transaction by hash.
eth_getTransaction :: forall p e . IsAsyncProvider p => HexString -> Web3 p e Transaction
eth_getTransaction hx = unsafeCoerceWeb3 $ remote "eth_getTransactionByHash" hx :: Web3 p () Transaction

-- | Call a function on a particular block's state root.
eth_call :: forall p e . IsAsyncProvider p => TransactionOptions -> ChainCursor -> Web3 p e HexString
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

-- | Creates a filter in the node, to notify when a new block arrives. 
-- | To check if the state has changed, call `eth_getFilterChanges`.
eth_newBlockFilter :: forall p e. IsAsyncProvider p => Web3 p e FilterId
eth_newBlockFilter = unsafeCoerceWeb3 $ remote "eth_newBlockFilter" :: Web3 p () FilterId

-- | Polling method for a filter, which returns an array of logs which occurred since last poll.
eth_getFilterChanges :: forall p e . IsAsyncProvider p => FilterId -> Web3 p e (Array Change)
eth_getFilterChanges fid = unsafeCoerceWeb3 $ remote "eth_getFilterChanges" fid :: Web3 p () (Array Change)

-- | Returns an array of all logs matching filter with given id.
eth_getFilterLogs :: forall p e. IsAsyncProvider p => FilterId -> Web3 p e (Array Change)
eth_getFilterLogs fid = unsafeCoerceWeb3 $ remote "eth_getFilterLogs" fid :: Web3 p () (Array Change)

-- | Returns an array of all logs matching a given filter object
eth_getLogs :: forall p e. IsAsyncProvider p => Filter -> Web3 p e (Array Change)
eth_getLogs filter = unsafeCoerceWeb3 $ remote "eth_getLogs" filter :: Web3 p () (Array Change)

-- | Uninstalls a filter with given id. Should always be called when watch is no longer needed.
eth_uninstallFilter :: forall p e . IsAsyncProvider p => FilterId -> Web3 p e Boolean
eth_uninstallFilter fid = unsafeCoerceWeb3 $ remote "eth_uninstallFilter" fid :: Web3 p () Boolean

personal_sign :: forall p e . IsAsyncProvider p => Address -> HexString -> Web3 p e HexString
personal_sign signer _data = unsafeCoerceWeb3 $ remote "personal_sign" signer _data :: Web3 p () HexString

