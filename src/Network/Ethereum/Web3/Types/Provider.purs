module Network.Ethereum.Web3.Types.Provider where

import Effect (Effect)

-- | Represents a connection to an ethereum client
foreign import data Provider :: Type

-- | Produces reference to Metamask provider
foreign import metamaskProvider :: Effect Provider

-- | Connect to an ethereum client at a given address, eg "http://localhost:8545"
foreign import httpProvider :: String -> Effect Provider
