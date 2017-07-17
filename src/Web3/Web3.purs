module Web3.Web3 where

import Prelude
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)

import Web3.Utils.Types (Address, BlockNumber)
import Web3.Utils.BigNumber (BigNumber)


--------------------------------------------------------------------------------
-- * Web3 Object
--------------------------------------------------------------------------------
foreign import data Web3 :: Type

foreign import newWeb3 :: forall eff . EffFn1 (eth :: ETH | eff) String Web3

--------------------------------------------------------------------------------
-- * Web3T
--------------------------------------------------------------------------------

foreign import data ETH :: Effect

data Web3T m a = Web3T (ReaderT Web3 m a)

foreign import web3ShowImpl :: Web3 -> String

instance showWeb3 :: Show Web3 where
  show = web3ShowImpl

unWeb3 :: forall m a . Web3T m a -> ReaderT Web3 m a
unWeb3 (Web3T action) = action

execWeb3 :: forall eff m a . MonadEff (eth :: ETH | eff) m
         => String
         -> Web3T m a
         -> m a
execWeb3 provider = \(Web3T action) -> do
  web3 <- liftEff $ runEffFn1 newWeb3 provider
  runReaderT action web3

-- instances

instance web3MFunctor :: Functor m => Functor (Web3T m) where
  map f (Web3T m) = Web3T (map f m)

instance web3MApply :: Apply m => Apply (Web3T m) where
  apply (Web3T f) (Web3T m) = Web3T (apply f m)

instance web3MApplicative :: Applicative m => Applicative (Web3T m) where
  pure = Web3T <<< pure

instance web3MBind :: Bind m => Bind (Web3T m) where
  bind (Web3T m) f = Web3T (m >>= (unWeb3 <<< f))

instance web3MMonad :: (Applicative m, Bind m) => Monad (Web3T m)

instance web3MEff :: MonadEff eff m => MonadEff eff (Web3T m) where
  liftEff = Web3T <<< liftEff

instance web3Reader :: Monad m => MonadAsk Web3 (Web3T m) where
  ask = Web3T ask

--------------------------------------------------------------------------------
-- * Eth
--------------------------------------------------------------------------------

getBalance :: forall eff m. MonadEff (eth :: ETH | eff) m => Address -> BlockNumber -> Web3T m BigNumber
getBalance addr bn = do
  web3 <- ask
  liftEff $ runEffFn2 (_getBalance web3) addr (show bn)

foreign import _getBalance :: forall eff . Web3 -> EffFn2 (eth :: ETH | eff) Address String BigNumber

verboseGetBalance :: forall eff .
                      Address
                   -> BlockNumber
                   -> Web3T (Eff (eth :: ETH , console :: CONSOLE | eff)) BigNumber
verboseGetBalance addr bn = do
  b <- getBalance addr bn
  liftEff $ logShow <<< show $ b
  pure b

isConnected :: forall eff m. MonadEff (eth :: ETH | eff) m => Web3T m Boolean
isConnected = do
  web3 <- ask
  liftEff $ runEffFn1 _isConnected web3

foreign import _isConnected :: forall eff . EffFn1 (eth :: ETH | eff) Web3 Boolean
