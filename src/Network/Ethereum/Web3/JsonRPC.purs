module Network.Ethereum.Web3.JsonRPC where

import Prelude

import Control.Monad.Aff (Aff, Error, attempt, error)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Compat (fromEffFnAff, EffFnAff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.Generic (defaultOptions, genericEncodeJSON)
import Data.Monoid (mempty)
import Network.Ethereum.Web3.Types (ETH, MethodName, Request, Response(..), Web3, Web3Error(..), mkRequest)
import Network.Ethereum.Web3.Types.Provider (Provider)


--------------------------------------------------------------------------------
-- * Asynchronous RPC Calls
--------------------------------------------------------------------------------

-- | Class representing a builder for a Web3 query
class Remote eff a | a -> eff where
  remote_ :: (Provider -> Array Foreign -> Aff (eth :: ETH | eff) Foreign) -> a

instance remoteBase :: (Decode a) => Remote eff (Web3 eff a) where
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

instance remoteInductive :: (Encode a, Remote eff b) => Remote eff (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (encode x : args)

foreign import _sendAsync :: forall eff. Provider -> Request -> EffFnAff (eth :: ETH |eff) Foreign

-- | Execute the Web3 query constructed inductively by the builder
remote :: forall a eff. Remote eff a => MethodName -> a
remote n = remote_ $ \provider ps -> fromEffFnAff $ _sendAsync provider $ mkRequest n 1 ps
