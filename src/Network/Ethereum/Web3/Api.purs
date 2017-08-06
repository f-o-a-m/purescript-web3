module Network.Ethereum.Web3.Api where


import Network.Ethereum.Web3.Types (Web3M, Address, BigNumber, Block, CallMode)
import Network.Ethereum.Web3.JsonRPC (remote)


type Web3 a = Web3M () a

-- | Returns current node version string.
web3_clientVersion :: Web3 String
web3_clientVersion = remote "web3_clientVersion"

-- | Returns the balance of the account of given address.
eth_getBalance :: Address -> CallMode -> Web3 BigNumber
eth_getBalance = remote "eth_getBalance"

-- | Returns information about a block by hash.
-- Use the boolean flag =true if you want the whole transaction objects
-- in the 'transactions' field, =false to get just the hash
eth_getBlockByNumber :: CallMode -> Boolean -> Web3 Block
eth_getBlockByNumber = remote "eth_getBlockByNumber"
