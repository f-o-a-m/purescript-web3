module Network.Ethereum.Web3.Api where

import Network.Ethereum.Web3.JsonRPC (remote)
import Network.Ethereum.Types (Address, HexString, BigNumber)
import Network.Ethereum.Web3.Types (Block, BlockNumber, ChainCursor, Change, FalseOrObject, Filter, FilterId, NoPay, SyncStatus, Transaction, TransactionOptions, TransactionReceipt, Web3, Wei)
import Type.Data.Boolean (kind Boolean)

-- | Returns current node version string.
web3_clientVersion :: forall e . Web3 e String
web3_clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (*not* the standardized SHA3-256) of the given data.
web3_sha3 :: forall e. HexString -> Web3 e HexString
web3_sha3 hexInput = remote "web3_sha3" hexInput

-- | Get the network id that the node is listening to.
net_version :: forall e . Web3 e BigNumber
net_version = remote "net_version"

-- | Returns `true`` if client is actively listening for network connections
net_listening :: forall e . Web3 e Boolean
net_listening = remote "net_listening"

-- | Get the number of currently connected peers.
net_getPeerCount :: forall e. Web3 e BigNumber
net_getPeerCount = remote "net_peerCount"

-- | Returns the current ethereum protocol version
eth_protocolVersion :: forall e. Web3 e String
eth_protocolVersion = remote "eth_protocolVersion"

-- | Get the sync status of the node.
eth_getSyncing :: forall e. Web3 e (FalseOrObject SyncStatus)
eth_getSyncing = remote "eth_syncing"

-- | Returns the client coinbase address
eth_coinbase :: forall e. Web3 e Address
eth_coinbase = remote "eth_coinbase"

-- | Returns `true` if client is actively mining new blocks
eth_mining :: forall e. Web3 e Boolean
eth_mining = remote "eth_mining"

-- | Returns the number of hashes per second that the node is mining with
eth_hashrate :: forall e. Web3 e BigNumber
eth_hashrate = remote "eth_hashrate"

-- | Returns the current price per gas in wei
eth_gasPrice :: forall e. Web3 e BigNumber
eth_gasPrice = remote "eth_gasPrice"

-- | Returns the number of most recent block
eth_blockNumber :: forall e. Web3 e BlockNumber
eth_blockNumber = remote "eth_blockNumber"

-- | Returns the balance of the account of given address.
eth_getBalance :: forall e . Address -> ChainCursor -> Web3 e BigNumber
eth_getBalance addr cm = remote "eth_getBalance" addr cm

-- | Returns the value from a storage position at a given address
eth_getStorageAt :: forall e. Address -> BigNumber -> ChainCursor -> Web3 e HexString
eth_getStorageAt addr storagePos cm = remote "eth_getStorageAt" addr storagePos cm

-- | Returns the number of transactions *sent* from an address
eth_getTransactionCount :: forall e. Address -> ChainCursor -> Web3 e BigNumber
eth_getTransactionCount addr cm = remote "eth_getTransactionCount" addr cm

-- | Returns the number of transactions in a block from a block matching the given block hash
eth_getBlockTransactionCountByHash :: forall e. HexString -> Web3 e BigNumber
eth_getBlockTransactionCountByHash blockHash = remote "eth_getBlockTransactionCountByHash" blockHash

-- | Returns the number of transactions in a block matching the given block number
eth_getBlockTransactionCountByNumber :: forall e. ChainCursor -> Web3 e BigNumber
eth_getBlockTransactionCountByNumber cm = remote "eth_getBlockTransactionCountByNumber" cm

-- TODO - is it appropriate for these to be Ints?

-- | Returns the number of uncles in a block from a block matching the given block hash
eth_getUncleCountByBlockHash :: forall e. HexString -> Web3 e BigNumber
eth_getUncleCountByBlockHash blockNumber = remote "eth_getUncleCountByBlockHash" blockNumber

-- | Returns the number of uncles in a block from a block matching the given block number
eth_getUncleCountByBlockNumber :: forall e. ChainCursor -> Web3 e BigNumber
eth_getUncleCountByBlockNumber cm = remote "eth_getUncleCountByBlockNumber" cm

-- | Returns code at a given address
eth_getCode :: forall e. Address -> ChainCursor -> Web3 e HexString
eth_getCode addr cm = remote "eth_getCode" addr cm

-- | The sign method calculates an Ethereum specific signature with: `sign(keccak256("\x19Ethereum Signed Message:\n" + len(message) + message)))`.
-- | By adding a prefix to the message makes the calculated signature recognisable as an Ethereum specific signature. This prevents misuse where a malicious DApp can sign arbitrary data (e.g. transaction) and use the signature to impersonate the victim.
-- | **Note** the address to sign with must be unlocked.
eth_sign :: forall e. Warn "eth_sign is deprecated in favor of personal_sign" => Address -> HexString -> Web3 e HexString
eth_sign addr msg = remote "eth_sign" addr msg

-- | Creates new message call transaction or a contract creation for signed transactions
eth_sendRawTransaction :: forall e. HexString -> Web3 e HexString
eth_sendRawTransaction rawTx = remote "eth_sendRawTransaction" rawTx

-- | Makes a call or transaction, which won't be added to the blockchain and returns the used gas, which can be used for estimating the used gas.
eth_estimateGas :: forall e. TransactionOptions Wei -> ChainCursor -> Web3 e BigNumber
eth_estimateGas txOpts cm = remote "eth_estimateGas" txOpts cm

-- | Returns information about a transaction by block hash and transaction index position.
eth_getTransactionByBlockHashAndIndex :: forall e. HexString -> BigNumber -> Web3 e Transaction
eth_getTransactionByBlockHashAndIndex blockHash txIndex = remote "eth_getTransactionByBlockHashAndIndex" blockHash txIndex

-- | Returns information about a transaction by block number and transaction index position.
eth_getTransactionByBlockNumberAndIndex :: forall e. ChainCursor -> BigNumber -> Web3 e Transaction
eth_getTransactionByBlockNumberAndIndex cm txIndex = remote "eth_getTransactionByBlockNumberAndIndex" cm txIndex

-- | Returns the receipt of a transaction by transaction hash.
eth_getTransactionReceipt :: forall e. HexString -> Web3 e TransactionReceipt
eth_getTransactionReceipt txHash = remote "eth_getTransactionReceipt" txHash

-- | Returns information about a uncle of a block by hash and uncle index position.
eth_getUncleByBlockHashAndIndex :: forall e. HexString -> BigNumber -> Web3 e Block
eth_getUncleByBlockHashAndIndex blockHash uncleIndex = remote "eth_getUncleByBlockHashAndIndex" blockHash uncleIndex

-- | Returns information about a uncle of a block by number and uncle index position.
eth_getUncleByBlockNumberAndIndex :: forall e. ChainCursor -> BigNumber -> Web3 e Block
eth_getUncleByBlockNumberAndIndex cm uncleIndex = remote "eth_getUncleByBlockNumberAndIndex" cm uncleIndex

-- | Returns a list of available compilers in the client.
eth_getCompilers :: forall e. Web3 e (Array String)
eth_getCompilers = remote "eth_getCompilers"

-- TODO: As the ABI is returned decoding this isn't trivial - not going to implement without a need
-- -- | Returns compiled solidity code.
-- eth_compileSolidity :: forall e. String -> Web3 e HexString
-- eth_compileSolidity code = remote "eth_compileSolidity"

-- | Returns information about a block by number.
eth_getBlockByNumber :: forall e . ChainCursor -> Web3 e Block
eth_getBlockByNumber cm = remote "eth_getBlockByNumber" cm false

-- | Returns information about a block by hash.
eth_getBlockByHash :: forall e . HexString -> Web3 e Block
eth_getBlockByHash hx = remote "eth_getBlockByHash" hx false

-- | Returns information about a transaction by hash.
eth_getTransaction :: forall e . HexString -> Web3 e Transaction
eth_getTransaction hx = remote "eth_getTransactionByHash" hx

-- | Call a function on a particular block's state root.
eth_call :: forall e . TransactionOptions NoPay -> ChainCursor -> Web3 e HexString
eth_call opts cm = remote "eth_call" opts cm

-- | Creates new message call transaction or a contract creation, if the data field contains code.
eth_sendTransaction :: forall e . TransactionOptions Wei -> Web3 e HexString
eth_sendTransaction opts = remote "eth_sendTransaction" opts

-- | Get all account addresses registered at the `Provider`
eth_getAccounts :: forall e . Web3 e (Array Address)
eth_getAccounts = remote "eth_accounts"

-- | Creates a filter object, based on filter options, to notify when the
-- | state changes (logs). To check if the state has changed, call 'eth_getFilterChanges'.
eth_newFilter :: forall e . Filter -> Web3 e FilterId
eth_newFilter f = remote "eth_newFilter" f

-- | Creates a filter in the node, to notify when a new block arrives.
-- | To check if the state has changed, call `eth_getFilterChanges`.
eth_newBlockFilter :: forall e. Web3 e FilterId
eth_newBlockFilter = remote "eth_newBlockFilter"

-- | Polling method for a filter, which returns an array of logs which occurred since last poll.
eth_getFilterChanges :: forall e . FilterId -> Web3 e (Array Change)
eth_getFilterChanges fid = remote "eth_getFilterChanges" fid

-- | Returns an array of all logs matching filter with given id.
eth_getFilterLogs :: forall e. FilterId -> Web3 e (Array Change)
eth_getFilterLogs fid = remote "eth_getFilterLogs" fid

-- | Returns an array of all logs matching a given filter object
eth_getLogs :: forall e. Filter -> Web3 e (Array Change)
eth_getLogs filter = remote "eth_getLogs" filter

-- | Uninstalls a filter with given id. Should always be called when watch is no longer needed.
eth_uninstallFilter :: forall e . FilterId -> Web3 e Boolean
eth_uninstallFilter fid = remote "eth_uninstallFilter" fid

-- | Sign a message with the given address, returning the signature.
personal_sign :: forall e . HexString -> Address -> Web3 e HexString
personal_sign _data signer = remote "personal_sign" _data signer

-- | Recover the address that signed the message.
personal_ecRecover :: forall e . HexString -> HexString -> Web3 e Address
personal_ecRecover _data sig = remote "personal_ecRecover" _data sig
