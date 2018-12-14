module Network.Ethereum.Web3.Api where

import Data.Maybe (Maybe, fromMaybe)
import Network.Ethereum.Types (Address, HexString, BigNumber)
import Network.Ethereum.Web3.JsonRPC (remote)
import Network.Ethereum.Web3.Types (Block, BlockNumber, ChainCursor, Change, FalseOrObject, Filter, FilterId, NoPay, SyncStatus, Transaction, TransactionOptions, TransactionReceipt, Web3)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Type.Data.Boolean (kind Boolean)

-- | Returns current node version string.
web3_clientVersion :: Partial => Web3 String
web3_clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (*not* the standardized SHA3-256) of the given data.
web3_sha3 :: Partial => HexString -> Web3 HexString
web3_sha3 hexInput = remote "web3_sha3" hexInput

-- | Get the network id that the node is listening to.
net_version :: Web3 String
net_version = remote "net_version"

-- | Returns `true`` if client is actively listening for network connections
net_listening :: Web3 Boolean
net_listening = remote "net_listening"

-- | Get the number of currently connected peers.
net_getPeerCount :: Web3 BigNumber
net_getPeerCount = remote "net_peerCount"

-- | Returns the current ethereum protocol version
eth_protocolVersion :: Web3 String
eth_protocolVersion = remote "eth_protocolVersion"

-- | Get the sync status of the node.
eth_getSyncing :: Web3 (FalseOrObject SyncStatus)
eth_getSyncing = remote "eth_syncing"

-- | Returns the client coinbase address
eth_coinbase :: Web3 Address
eth_coinbase = remote "eth_coinbase"

-- | Returns `true` if client is actively mining new blocks
eth_mining :: Web3 Boolean
eth_mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with
eth_hashrate :: Web3 BigNumber
eth_hashrate = remote "eth_hashrate"

-- | Returns the current price per gas in wei
eth_gasPrice :: Web3 BigNumber
eth_gasPrice = remote "eth_gasPrice"

-- | Returns the number of most recent block
eth_blockNumber :: Web3 BlockNumber
eth_blockNumber = remote "eth_blockNumber"

-- | Returns the balance of the account of given address.
eth_getBalance :: Address -> ChainCursor -> Web3 BigNumber
eth_getBalance addr cm = remote "eth_getBalance" addr cm

-- | Returns the value from a storage position at a given address
eth_getStorageAt :: Address -> BigNumber -> ChainCursor -> Web3 HexString
eth_getStorageAt addr storagePos cm = remote "eth_getStorageAt" addr storagePos cm

-- | Returns the number of transactions *sent* from an address
eth_getTransactionCount :: Address -> ChainCursor -> Web3 BigNumber
eth_getTransactionCount addr cm = remote "eth_getTransactionCount" addr cm

-- | Returns the number of transactions in a block from a block matching the given block hash
eth_getBlockTransactionCountByHash :: HexString -> Web3 BigNumber
eth_getBlockTransactionCountByHash blockHash = remote "eth_getBlockTransactionCountByHash" blockHash

-- | Returns the number of transactions in a block matching the given block number
eth_getBlockTransactionCountByNumber :: ChainCursor -> Web3 BigNumber
eth_getBlockTransactionCountByNumber cm = remote "eth_getBlockTransactionCountByNumber" cm

-- | Returns the number of uncles in a block from a block matching the given block hash
eth_getUncleCountByBlockHash :: HexString -> Web3 BigNumber
eth_getUncleCountByBlockHash blockNumber = remote "eth_getUncleCountByBlockHash" blockNumber

-- | Returns the number of uncles in a block from a block matching the given block number
eth_getUncleCountByBlockNumber :: ChainCursor -> Web3 BigNumber
eth_getUncleCountByBlockNumber cm = remote "eth_getUncleCountByBlockNumber" cm

-- | Returns code at a given address
eth_getCode :: Address -> ChainCursor -> Web3 HexString
eth_getCode addr cm = remote "eth_getCode" addr cm

-- | Creates new message call transaction or a contract creation for signed transactions
eth_sendRawTransaction :: HexString -> Web3 HexString
eth_sendRawTransaction rawTx = remote "eth_sendRawTransaction" rawTx

-- | Makes a call or transaction, which won't be added to the blockchain and returns the used gas, which can be used for estimating the used gas.
eth_estimateGas :: TransactionOptions MinorUnit -> Web3 BigNumber
eth_estimateGas txOpts = remote "eth_estimateGas" txOpts

-- | Returns information about a transaction by block hash and transaction index position.
eth_getTransactionByBlockHashAndIndex :: HexString -> BigNumber -> Web3 Transaction
eth_getTransactionByBlockHashAndIndex blockHash txIndex = remote "eth_getTransactionByBlockHashAndIndex" blockHash txIndex

-- | Returns information about a transaction by block number and transaction index position.
eth_getTransactionByBlockNumberAndIndex :: ChainCursor -> BigNumber -> Web3 Transaction
eth_getTransactionByBlockNumberAndIndex cm txIndex = remote "eth_getTransactionByBlockNumberAndIndex" cm txIndex

-- | Returns the receipt of a transaction by transaction hash.
eth_getTransactionReceipt :: HexString -> Web3 TransactionReceipt
eth_getTransactionReceipt txHash = remote "eth_getTransactionReceipt" txHash

-- | Returns information about a uncle of a block by hash and uncle index position.
eth_getUncleByBlockHashAndIndex :: HexString -> BigNumber -> Web3 Block
eth_getUncleByBlockHashAndIndex blockHash uncleIndex = remote "eth_getUncleByBlockHashAndIndex" blockHash uncleIndex

-- | Returns information about a uncle of a block by number and uncle index position.
eth_getUncleByBlockNumberAndIndex :: ChainCursor -> BigNumber -> Web3 Block
eth_getUncleByBlockNumberAndIndex cm uncleIndex = remote "eth_getUncleByBlockNumberAndIndex" cm uncleIndex

-- | Returns a list of available compilers in the client.
eth_getCompilers :: Partial => Web3 (Array String)
eth_getCompilers = remote "eth_getCompilers"

-- | Returns information about a block by number.
eth_getBlockByNumber :: ChainCursor -> Web3 Block
eth_getBlockByNumber cm = remote "eth_getBlockByNumber" cm false

-- | Returns information about a block by hash.
eth_getBlockByHash :: HexString -> Web3 Block
eth_getBlockByHash hx = remote "eth_getBlockByHash" hx false

-- | Returns information about a transaction by hash.
eth_getTransaction :: HexString -> Web3 Transaction
eth_getTransaction hx = remote "eth_getTransactionByHash" hx

-- | Call a function on a particular block's state root.
eth_call :: TransactionOptions NoPay -> ChainCursor -> Web3 HexString
eth_call opts cm = remote "eth_call" opts cm

-- | Creates new message call transaction or a contract creation, if the data field contains code.
eth_sendTransaction :: TransactionOptions MinorUnit -> Web3 HexString
eth_sendTransaction opts = remote "eth_sendTransaction" opts

-- | Get all account addresses registered at the `Provider`
eth_getAccounts :: Web3 (Array Address)
eth_getAccounts = remote "eth_accounts"

-- | Creates a filter object, based on filter options, to notify when the
-- | state changes (logs). To check if the state has changed, call 'eth_getFilterChanges'.
eth_newFilter :: forall a . Filter a -> Web3 FilterId
eth_newFilter f = remote "eth_newFilter" f

-- | Creates a filter in the node, to notify when a new block arrives.
-- | To check if the state has changed, call `eth_getFilterChanges`.
eth_newBlockFilter :: Web3 FilterId
eth_newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a filter, which returns an array of logs which occurred since last poll.
eth_getFilterChanges :: FilterId -> Web3 (Array Change)
eth_getFilterChanges fid = remote "eth_getFilterChanges" fid

-- | Returns an array of all logs matching filter with given id.
eth_getFilterLogs :: FilterId -> Web3 (Array Change)
eth_getFilterLogs fid = remote "eth_getFilterLogs" fid

-- | Returns an array of all logs matching a given filter object
eth_getLogs :: forall a. Filter a -> Web3 (Array Change)
eth_getLogs filter = remote "eth_getLogs" filter

-- | Uninstalls a filter with given id. Should always be called when watch is no longer needed.
eth_uninstallFilter :: FilterId -> Web3 Boolean
eth_uninstallFilter fid = remote "eth_uninstallFilter" fid

-- | Sign a message with the given address, returning the signature.
personal_sign :: HexString -> Address -> Maybe String -> Web3 HexString
personal_sign _data signer password = remote "personal_sign" _data signer (fromMaybe "" password)

-- | Recover the address that signed the message from (1) the message and (2) the signature
personal_ecRecover :: HexString -> HexString -> Web3 Address
personal_ecRecover _data sig = remote "personal_ecRecover" _data sig
