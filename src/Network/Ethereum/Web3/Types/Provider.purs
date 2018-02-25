module Network.Ethereum.Web3.Types.Provider where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

-- | Represents a connection to an ethereum client
foreign import data Provider :: Type

-- | Produces reference to Metamas provider
foreign import metamaskProvider :: forall e . Eff (exception :: EXCEPTION | e) Provider

-- | Connect to an ethereum client at a given address, eg "http://localhost:8545"
foreign import httpProvider :: forall e . String -> Eff e Provider
