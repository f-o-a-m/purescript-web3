module Network.Ethereum.Web3.Provider where

import Prelude

import Control.Monad.Aff (Aff, Fiber, forkAff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExceptT)
import Data.Either (Either)
import Network.Ethereum.Web3.Types (ETH, Web3(..), Web3Error)
import Type.Proxy (Proxy(..))

-- | Represents a connection to an ethereum client
foreign import data Provider :: Type

class IsAsyncProvider p where
  getAsyncProvider :: forall e . Web3 p e Provider

-- | Used only in the browser
data Metamask

metamask :: Proxy Metamask
metamask = Proxy

foreign import metamaskProvider :: forall e . Eff (exception :: EXCEPTION | e) Provider

instance providerMetamaskM :: IsAsyncProvider Metamask where
  getAsyncProvider = liftAff $ liftEff' metamaskProvider

-- | Connect to an ethereum client at a given address, eg "http://localhost:8545"
foreign import httpProvider :: forall e . String -> Eff e Provider

-- | Run an asynchronous `ETH` action
runWeb3 :: forall p e a . Proxy p -> Web3 p e a -> Aff (eth :: ETH | e) (Either Web3Error a)
runWeb3 _ (Web3 action) = runExceptT action

-- | Fork an asynchronous `ETH` action
forkWeb3 :: forall p e a .
            Proxy p
         -> Web3 p e a
         -> Aff (eth :: ETH | e) (Fiber (eth :: ETH | e) (Either Web3Error a))
forkWeb3 _ (Web3 action) = forkAff <<< runExceptT $ action

-- | Fork an asynchronous `ETH` action inside Web3 monad
forkWeb3' :: forall p e a .
             Proxy p
          -> Web3 p e a
          -> Web3 p e (Fiber (eth :: ETH | e) (Either Web3Error a))
forkWeb3' p web3Action = liftAff $ forkWeb3 p web3Action
