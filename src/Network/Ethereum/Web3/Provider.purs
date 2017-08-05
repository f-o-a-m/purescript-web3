module Network.Ethereum.Web3.Provider where

import Prelude
import Control.Monad.Eff (Eff)

import Network.Ethereum.Web3.Types (ETH)

foreign import data Provider :: Type

foreign import getProvider :: forall e . Eff (eth :: ETH | e) Provider

foreign import showProviderImpl :: Provider -> String

instance showProvider :: Show Provider where
  show = showProviderImpl
