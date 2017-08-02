module Network.Ethereum.Web3.JsonRPC where

import Prelude
import Data.Array ((:))
import Data.Monoid (mempty)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Control.Monad.Aff (Aff)
import Control.Monad
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Network.Ethereum.Web3.Types (ETH, Web3M(..))


class Remote e a where
  _remote :: (Array Foreign -> Web3M e Foreign) -> a

instance remoteBase :: Decode a => Remote e (Web3M e a) where
  _remote f = do
    res <- f mempty
    case runExcept <<< decode $ res of
      Left e -> throwError <<< error <<< show $ e
      Right r -> pure r

instance remoteInductive :: (Encode a, Remote e b) => Remote e (a -> b) where
  _remote f x = _remote $ \args -> f (encode x : args)
