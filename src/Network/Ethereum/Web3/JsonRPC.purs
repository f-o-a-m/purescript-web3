module Network.Ethereum.Web3.JsonRPC where

import Prelude
import Data.Array ((:))
import Data.Monoid (mempty)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Foreign.Generic (defaultOptions, genericEncode)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Network.Ethereum.Web3.Provider (Provider, getProvider)
import Network.Ethereum.Web3.Types (ETH, Web3M(..))


type MethodName = String

class Remote e a where
  remote_ :: (Provider -> Array Foreign -> Aff (eth :: ETH | e) Foreign) -> a

instance remoteBase :: Decode a => Remote e (Web3M e a) where
  remote_ f = do
    p <- liftEff getProvider
    res <- Web3M $ f p mempty
    case runExcept <<< decode $ res of
      -- TODO: fix bad error handling
      Left e -> throwError <<< error <<< show $ e
      Right r -> pure r

instance remoteInductive :: (Encode a, Remote e b) => Remote e (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (encode x : args)


foreign import web3Request :: Provider -> Request ->  Aff (eth :: ETH) Foreign

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
-- Try and figure out a way to put other @e@ here instead of ().
remote :: forall a . Remote () a => MethodName -> a
remote n = remote_ $ \provider ps -> web3Request provider $ mkRequest n 1 ps


data Request =
  Request { jsonrpc :: String
          , id :: Int
          , method :: MethodName
          , params :: Array Foreign
          }

derive instance genericRequest :: Generic Request _

instance encodeRequest :: Encode Request where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

mkRequest :: MethodName -> Int -> Array Foreign -> Request
mkRequest name reqId ps = Request { jsonrpc : "2.0"
                                  , id : 1
                                  , method : name
                                  , params : ps
                                  }
