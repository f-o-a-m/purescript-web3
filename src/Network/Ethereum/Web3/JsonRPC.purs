module Network.Ethereum.Web3.JsonRPC where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff, makeAff, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array ((:))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericEncode, genericDecode)
import Data.Foreign.Index (readProp)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid (mempty)
import Network.Ethereum.Web3.Types (ETH, Web3M(..), Web3MA(..), Provider)

type MethodName = String

--------------------------------------------------------------------------------
-- * Synchronous RPC Calls
--------------------------------------------------------------------------------

class Remote e a where
  remote_ :: (Provider -> Array Foreign -> Eff (eth :: ETH, exception :: EXCEPTION | e) Foreign) -> a

instance remoteBase :: Decode a => Remote e (Web3M e a) where
  remote_ f = do
    p <- ask
    res <- Web3M <<< lift $ f p mempty
    Web3M <<< lift <<< decodeResponse $ res

instance remoteInductive :: (Encode a, Remote e b) => Remote e (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (encode x : args)

foreign import _send :: Provider -> Request ->  Eff (eth :: ETH, exception :: EXCEPTION) Foreign

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
-- Try and figure out a way to put other @e@ here instead of ().
remote :: forall a . Remote () a => MethodName -> a
remote n = remote_ $ \provider ps -> _send provider $ mkRequest n 1 ps


--------------------------------------------------------------------------------
-- * Asynchronous RPC Calls
--------------------------------------------------------------------------------

class RemoteAsync e a where
  remoteAsync_ :: (Provider -> Array Foreign -> Aff (eth :: ETH | e) Foreign) -> a

instance remoteAsyncBase :: Decode a => RemoteAsync e (Web3MA e a) where
  remoteAsync_ f = do
    p <- ask
    res <- Web3MA <<< lift $ f p mempty
    Web3MA <<< lift $ do
      ea <- liftEff' <<< decodeResponse $ res
      either throwError pure ea

instance remoteAsyncInductive :: (Encode a, RemoteAsync e b) => RemoteAsync e (a -> b) where
  remoteAsync_ f x = remoteAsync_ $ \p args -> f p (encode x : args)

foreign import _sendAsync :: (Foreign -> Eff (eth :: ETH) Unit) -> Provider -> Request -> Eff (eth :: ETH) Unit

remoteAsync :: forall a . RemoteAsync () a => MethodName -> a
remoteAsync n = remoteAsync_ $ \provider ps -> makeAff (\error succ -> _sendAsync succ provider $ mkRequest n 1 ps)

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

decodeResponse :: forall e a . Decode a => Foreign -> Eff (exception :: EXCEPTION | e) a
decodeResponse a = do
    resp <- tryParse a
    case getResponse resp of
      Left err -> throw <<< show $ err
      Right f -> tryParse f

tryParse :: forall e a . Decode a => Foreign -> Eff (exception :: EXCEPTION | e) a
tryParse = either (throw <<< show) pure <<< runExcept <<< decode
