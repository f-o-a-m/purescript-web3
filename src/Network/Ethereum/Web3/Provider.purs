module Network.Ethereum.Web3.Provider where

import Prelude
import Control.Monad.Aff (Aff, Fiber, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.Ethereum.Web3.Types (ETH, Web3M(..), Web3MA(..))

foreign import data Provider :: Type

class IsSyncProvider p where
  getSyncProvider :: forall e . Web3M p e Provider

class IsAsyncProvider p where
  getAsyncProvider :: forall e . Web3MA p e Provider

data Metamask

foreign import metamaskProvider :: forall e . Eff e Provider

instance providerMetamaskM :: IsAsyncProvider Metamask where
  getAsyncProvider = Web3MA <<< liftEff $ metamaskProvider

data HttpProvider

foreign import httpProvider :: forall e . String -> Eff e Provider

runWeb3M :: forall p e a . Web3M p e a -> Eff (eth :: ETH , exception :: EXCEPTION | e) a
runWeb3M (Web3M action) = action

runWeb3MA :: forall p e a . Web3MA p e a -> Aff (eth :: ETH | e) a
runWeb3MA (Web3MA action) = action

forkWeb3MA :: forall p e a . Web3MA p e a -> Aff (eth :: ETH | e) (Fiber (eth :: ETH | e) a)
forkWeb3MA (Web3MA action) = forkAff action
