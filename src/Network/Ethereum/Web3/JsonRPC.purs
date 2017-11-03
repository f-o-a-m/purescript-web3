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
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Foreign.Index (readProp)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid (mempty)
import Network.Ethereum.Web3.Types (ETH, Web3(..))
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, Provider, getAsyncProvider)

type MethodName = String

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

-- | Web3 json RPC format
newtype Request =
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
                                  , id : reqId
                                  , method : name
                                  , params : ps
                                  }

data RpcError =
  RpcError { code     :: Int
           , message  :: String
           }

derive instance genericRpcError :: Generic RpcError _

instance showRpcError :: Show RpcError where
  show = genericShow

instance decodeRpcError :: Decode RpcError where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

newtype Response = Response (Either RpcError Foreign)

getResponse :: Response -> Either RpcError Foreign
getResponse (Response r) = r

instance decodeResponse' :: Decode Response where
  decode a =
    let errDecoder = readProp "error" a >>= decode
    in Response <$> ((Left <$> errDecoder) <|> (Right <$> readProp "result" a))

-- | Attempt to decode the response, throwing an Error in case of failure
decodeResponse :: forall e a . Decode a => Foreign -> Eff (exception :: EXCEPTION | e) a
decodeResponse a = do
    resp <- tryParse a
    case getResponse resp of
      Left err -> throw <<< show $ err
      Right f -> tryParse f

tryParse :: forall e a . Decode a => Foreign -> Eff (exception :: EXCEPTION | e) a
tryParse = either (throw <<< show) pure <<< runExcept <<< decode
