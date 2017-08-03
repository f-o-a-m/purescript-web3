module Network.Ethereum.Web3.JsonRPC where

import Prelude
import Data.Array ((:))
import Data.Monoid (mempty)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Generic.Rep (class Generic)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Control.Monad.Aff (Aff)
import Control.Monad
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Network.Ethereum.Web3.Provider (Provider, getProvider)
import Network.Ethereum.Web3.Types (ETH, Web3M(..))


type MethodName = String

class Remote e a where
  _remote :: (Provider -> Array Foreign -> Web3M e Foreign) -> a

instance remoteBase :: Decode a => Remote e (Web3M e a) where
  _remote f = do
    p <- liftEff getProvider
    res <- f p mempty
    case runExcept <<< decode $ res of
      -- TODO: fix bad error handling
      Left e -> throwError <<< error <<< show $ e
      Right r -> pure r

instance remoteInductive :: (Encode a, Remote e b) => Remote e (a -> b) where
  _remote f x = _remote $ \p args -> f p (encode x : args)

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
--remote :: Remote a => MethodName -> a
--remote n = remote_ call
--  where
--    call uri = connection uri . encode . Request n 1
--    connection uri body = do
--        manager <- newManager tlsManagerSettings
--        request <- parseRequest uri
--        let request' = request
--                     { requestBody = RequestBodyLBS body
--                     , requestHeaders = [("Content-Type", "application/json")]
--                     , method = "POST" }
--        responseBody <$> httpLbs request' manager

data Request =
  Request { jsonrpc :: String
          , id :: Int
          , method :: MethodName
          , params :: Array Foreign
          }

derive instance genericRequest :: Generic Request _

instance encodeRequest :: Encode Request where
  encode x = genericEncode (defaultOptions { unwrapSingleConstructors = true }) x

mkRequest :: MethodName -> Array Foreign -> Request
mkRequest name ps = Request { jsonrpc : "2.0"
                            , id : 1
                            , method : name
                            , params : ps
                            }
