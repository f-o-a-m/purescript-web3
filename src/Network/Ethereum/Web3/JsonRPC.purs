module Network.Ethereum.Web3.JsonRPC where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Compat (fromEffFnAff, EffFnAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Except (runExcept)
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, ForeignError(..), fail, isNull, readString)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Foreign.Index (readProp)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid (mempty)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, getAsyncProvider)
import Network.Ethereum.Web3.Types (ETH, Web3(..))


--------------------------------------------------------------------------------
-- * Asynchronous RPC Calls
--------------------------------------------------------------------------------

-- | Class representing a builder for a Web3 query
class Remote e a where
  remote_ :: (Provider -> Array Foreign -> Aff (eth :: ETH | e) Foreign) -> a

instance remoteBase :: (IsAsyncProvider p, Decode a) => Remote e (Web3 p e a) where
  remote_ f = do
    p <- getAsyncProvider
    res <- Web3 $ f p mempty
    Web3 $ liftEff' <<< decodeResponse $ res

instance remoteInductive :: (Encode a, Remote e b) => Remote e (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (encode x : args)

foreign import _sendAsync :: Provider -> Request -> EffFnAff (eth :: ETH) Foreign

-- | Execute the Web3 query constructed inductively by the builder
remote :: forall a . Remote () a => MethodName -> a
remote n = remote_ $ \provider ps -> fromEffFnAff $ _sendAsync provider $ mkRequest n 1 ps

