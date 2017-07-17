module Web3.Web3 where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Function.Uncurried (Fn2, runFn2)

import Web3.Utils.Types (Address, BlockNumber)
import Web3.Utils.BigNumber (BigNumber)


--------------------------------------------------------------------------------
-- * Web3 Object
--------------------------------------------------------------------------------
foreign import data Web3 :: Type

foreign import newWeb3 :: String -> Web3

--------------------------------------------------------------------------------
-- * Web3M
--------------------------------------------------------------------------------

foreign import data ETH :: Effect

data Web3M m a = Web3M (ReaderT Web3 m a)

runWeb3 :: forall m a . Web3M m a -> ReaderT Web3 m a
runWeb3 (Web3M action) = action

execWeb3 :: String -> (forall m a. Monad m => Web3M m a -> m a)
execWeb3 provider = \(Web3M action) ->
  let web3 = newWeb3 provider
  in runReaderT action web3

-- instances

instance web3MFunctor :: Functor m => Functor (Web3M m) where
  map f (Web3M m) = Web3M (map f m)

instance web3MApply :: Apply m => Apply (Web3M m) where
  apply (Web3M f) (Web3M m) = Web3M (apply f m)

instance web3MApplicative :: Applicative m => Applicative (Web3M m) where
  pure = Web3M <<< pure

instance web3MBind :: Bind m => Bind (Web3M m) where
  bind (Web3M m) f = Web3M (m >>= (runWeb3 <<< f))

instance web3MMonad :: (Applicative m, Bind m) => Monad (Web3M m)

instance web3MEff :: MonadEff eff m => MonadEff eff (Web3M m) where
  liftEff = Web3M <<< liftEff

--------------------------------------------------------------------------------
-- * Eth
--------------------------------------------------------------------------------

getBalance :: forall eff m. MonadEff (eth :: ETH | eff) m => Address -> BlockNumber -> Web3M m BigNumber
getBalance addr bn = runFn2 _getBalance addr (show bn)

foreign import _getBalance :: forall m . Fn2 Address String (Web3M m BigNumber)

verboseGetBalance :: forall eff .
                      Address
                   -> BlockNumber
                   -> Web3M (Eff (eth :: ETH , console :: CONSOLE | eff)) BigNumber
verboseGetBalance addr bn = do
  b <- getBalance addr bn
  liftEff $ logShow <<< show $ b
  pure b
