module Network.Ethereum.Web3.JsonRPC where

import Prelude

import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Compat (fromEffFnAff, EffFnAff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Monoid (mempty)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, getAsyncProvider)
import Network.Ethereum.Web3.Types (ETH, Web3, Request, Response(..), MethodName, mkRequest)


--------------------------------------------------------------------------------
-- * Asynchronous RPC Calls
--------------------------------------------------------------------------------

-- | Class representing a builder for a Web3 query
class Remote e a where
  remote_ :: (Provider -> Array Foreign -> Aff (eth :: ETH | e) Foreign) -> a

instance remoteBase :: (IsAsyncProvider p, Decode a) => Remote e (Web3 p e a) where
  remote_ f = do
    p <- getAsyncProvider
    res <- liftAff $ f p mempty
    case runExcept $ decode res of
      -- case where we get either a known Web3Error or a foreign value
      Right (Response r) -> case r of
        Left err -> throwError err
        Right a -> pure a
      Left err -> liftAff <<< liftEff' $ throw $ "Parser error : " <> show err

instance remoteInductive :: (Encode a, Remote e b) => Remote e (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (encode x : args)

foreign import _sendAsync :: Provider -> Request -> EffFnAff (eth :: ETH) Foreign

-- | Execute the Web3 query constructed inductively by the builder
remote :: forall a . Remote () a => MethodName -> a
remote n = remote_ $ \provider ps -> fromEffFnAff $ _sendAsync provider $ mkRequest n 1 ps
