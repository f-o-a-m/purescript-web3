module Network.Ethereum.Web3.Api where


import Network.Ethereum.Web3.Types (Web3M, Address, BigNumber)
import Network.Ethereum.Web3.JsonRPC (remote)


type Web3 a = Web3M () a

-- | Returns current node version string.
web3_clientVersion :: Web3 String
web3_clientVersion = remote "web3_clientVersion"

-- | Returns Keccak-256 (not the standardized SHA3-256) of the given data.
web3_sha3 :: String -> Web3 String
{-# INLINE web3_sha3 #-}
web3_sha3 = remote "web3_sha3"

-- | Returns the balance of the account of given address.
eth_getBalance :: Address -> Web3 BigNumber
eth_getBalance = remote "eth_getBalance"
