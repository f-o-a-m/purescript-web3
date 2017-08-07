module Network.Ethereum.Web3.Api where


import Network.Ethereum.Web3.Types (Web3M, Web3MA, Address, BigNumber, Block, CallMode, HexString, Transaction, TransactionOptions)
import Network.Ethereum.Web3.JsonRPC (remote)


type Web3 a = Web3M () a

type Web3A a = Web3MA () a

-- | Returns current node version string.
web3_clientVersion :: Web3 String
web3_clientVersion = remote "web3_clientVersion"

-- | Returns the balance of the account of given address.
eth_getBalance :: Address -> CallMode -> Web3 BigNumber
eth_getBalance = remote "eth_getBalance"

-- | Returns information about a block by hash.
-- Use the boolean flag =true if you want the whole transaction objects
-- in the 'transactions' field, =false to get just the hash
eth_getBlockByNumber :: CallMode -> Web3 Block
eth_getBlockByNumber cm = remote "eth_getBlockByNumber" cm false

eth_getBlockByHash :: HexString -> Web3 Block
eth_getBlockByHash = remote "eth_getBlockByHash"

eth_getTransaction :: HexString -> Web3 Transaction
eth_getTransaction = remote "eth_getTransactionByHash"

-- | Executes a new message call immediately without creating a
-- transaction on the block chain.
eth_call :: TransactionOptions -> CallMode -> Web3 HexString
eth_call = remote "eth_call"

-- | Creates new message call transaction or a contract creation,
-- if the data field contains code.
eth_sendTransaction :: TransactionOptions -> Web3 HexString
eth_sendTransaction = remote "eth_sendTransaction"
