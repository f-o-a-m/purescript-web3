module Network.Ethereum.Web3.JsonRPC where

import Prelude

import Effect.Aff (Aff, Error, attempt, error)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Array ((:))
import Data.Either (Either(..))
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (defaultOptions, genericEncodeJSON)
import Network.Ethereum.Web3.Types (MethodName, Request, Response(..), Web3, Web3Error(..), mkRequest)
import Network.Ethereum.Web3.Types.Provider (Provider)


--------------------------------------------------------------------------------
-- * Asynchronous RPC Calls
--------------------------------------------------------------------------------

-- | Class representing a builder for a Web3 query
class Remote a where
  remote_ :: (Provider -> Array Foreign -> Aff Foreign) -> a

instance remoteBase :: (Decode a) => Remote (Web3 a) where
  remote_ f = do
    p <- ask
    res' <- liftAff $ attempt $ f p mempty
    case res' of
      Left uncheckedErr -> throwError $ asError $ RemoteError $ show uncheckedErr
      Right res -> case runExcept $ decode res of
        -- case where we get either a known Web3Error or a foreign value
        Left err -> throwError $ asError $ ParserError $ show err
        Right (Response r) -> case r of
          Left err -> throwError $ asError err
          Right a -> pure a
    where
      -- NOTE: this is a bit hacky
      -- see Network.Ethereum.Web3.Types.Types#parseMsg
      asError :: Web3Error -> Error
      asError e = error $ genericEncodeJSON defaultOptions e

instance remoteInductive :: (Encode a, Remote b) => Remote (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (encode x : args)

foreign import _sendAsync :: Provider -> Request -> EffectFnAff Foreign

-- | Execute the Web3 query constructed inductively by the builder
remote :: forall a. Remote a => MethodName -> a
remote n = remote_ $ \provider ps -> fromEffectFnAff $ _sendAsync provider $ mkRequest n 1 ps
