module Network.Ethereum.Web3.JsonRPC where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Data.Array ((:))
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)
import Foreign (Foreign)
import Network.Ethereum.Web3.Types (MethodName, Request, Response(..), Web3, Web3Error(..), mkRequest, throwWeb3)
import Network.Ethereum.Web3.Types.Provider (Provider)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

--------------------------------------------------------------------------------
-- * Asynchronous RPC Calls
--------------------------------------------------------------------------------
-- | Class representing a builder for a Web3 query
class Remote a where
  remote_ :: (Provider -> Array Foreign -> Aff Foreign) -> a

instance ReadForeign a => Remote (Web3 a) where
  remote_ f = do
    p <- ask
    res' <- liftAff $ attempt $ f p mempty
    case res' of
      Left uncheckedErr -> throwWeb3 $ RemoteError (show uncheckedErr)
      Right res -> case runExcept $ readImpl res of
        -- case where we get either a known Web3Error or a foreign value
        Left err -> throwWeb3 $ ParserError $ show err
        Right (Response r) -> case r of
          Left err -> throwWeb3 err
          Right a -> pure a

instance (WriteForeign a, Remote b) => Remote (a -> b) where
  remote_ f x = remote_ $ \p args -> f p (writeImpl x : args)

foreign import _sendAsync :: Provider -> Request -> EffectFnAff Foreign

-- | Execute the Web3 query constructed inductively by the builder
remote :: forall a. Remote a => MethodName -> a
remote n = remote_ $ \provider ps -> fromEffectFnAff $ _sendAsync provider $ mkRequest n 1 ps
