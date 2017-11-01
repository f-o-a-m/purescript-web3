module Network.Ethereum.Web3.Provider where

import Prelude
import Control.Monad.Aff (Aff, Fiber, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Network.Ethereum.Web3.Types (ETH, Web3(..))

foreign import data Provider :: Type

class IsAsyncProvider p where
  getAsyncProvider :: forall e . Web3 p e Provider

data Metamask

foreign import metamaskProvider :: forall e . Eff e Provider

instance providerMetamaskM :: IsAsyncProvider Metamask where
  getAsyncProvider = Web3 <<< liftEff $ metamaskProvider

foreign import httpProvider :: forall e . String -> Eff e Provider

runWeb3 :: forall p e a . Web3 p e a -> Aff (eth :: ETH | e) a
runWeb3 (Web3 action) = action

forkWeb3 :: forall p e a . Web3 p e a -> Aff (eth :: ETH | e) (Fiber (eth :: ETH | e) a)
forkWeb3 (Web3 action) = forkAff action
