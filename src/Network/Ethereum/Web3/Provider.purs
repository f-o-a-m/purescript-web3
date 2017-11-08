module Network.Ethereum.Web3.Provider where

import Prelude
import Control.Monad.Aff (Aff, Fiber, forkAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Network.Ethereum.Web3.Types (ETH, Web3(..))
import Type.Proxy (Proxy(..))

-- | Represents a connection to an ethereum client
foreign import data Provider :: Type

class IsAsyncProvider p where
  getAsyncProvider :: forall e . Web3 p e Provider

-- | Used only in the browser
data Metamask

metamask :: Proxy Metamask
metamask = Proxy

foreign import metamaskProvider :: forall e . Eff e Provider

instance providerMetamaskM :: IsAsyncProvider Metamask where
  getAsyncProvider = Web3 <<< liftEff $ metamaskProvider

-- | Connect to an ethereum client at a given address, eg "http://localhost:8545"
foreign import httpProvider :: forall e . String -> Eff e Provider

-- | Run an asynchronous `ETH` action
runWeb3 :: forall p e a . Proxy p -> Web3 p e a -> Aff (eth :: ETH | e) a
runWeb3 _ (Web3 action) = action

-- | Fork an asynchronous `ETH` action
forkWeb3 :: forall p e a . Proxy p -> Web3 p e a -> Aff (eth :: ETH | e) (Fiber (eth :: ETH | e) a)
forkWeb3 _ (Web3 action) = forkAff action

-- | Fork an asynchronous `ETH` action inside Web3 monad
forkWeb3' :: forall p e a . Proxy p -> Web3 p e a -> Web3 p e (Fiber (eth :: ETH | e) a)
forkWeb3' _ (Web3 action) = Web3 $ forkAff action
