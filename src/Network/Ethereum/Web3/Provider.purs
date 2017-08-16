module Network.Ethereum.Web3.Provider where

import Control.Monad.Eff (Eff)
import Network.Ethereum.Web3.Types (ETH, Provider)

foreign import metamaskProvider :: forall e . Eff (eth :: ETH | e) Provider

foreign import httpProvider :: forall e . String -> Eff (eth :: ETH | e) Provider
