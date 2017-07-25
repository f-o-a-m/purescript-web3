module Web3.Web3 where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Data.Array (cons)
import Data.Monoid (mempty)
import Data.Maybe (Maybe(..))
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Either (Either(..))
import Control.Monad.Except (runExcept, fromRight)
import Partial.Unsafe (unsafePartial)

import Web3.Utils.Types (Address(..), HexString(..))
import Web3.Utils.Utils (BlockId)
import Web3.Utils.BigNumber (BigNumber)

--------------------------------------------------------------------------------
-- * Web3 Object
--------------------------------------------------------------------------------

foreign import data Web3Backend :: Type

foreign import newWeb3Backend :: forall eff . EffFn1 (eth :: ETH | eff) String Web3Backend

foreign import web3ShowImpl :: Web3Backend -> String

instance showWeb3Backend :: Show Web3Backend where
  show = web3ShowImpl

class Backend b where
  web3Object :: Web3 b Web3Backend

foreign import getWeb3Backend :: forall eff . Eff (eth :: ETH | eff) Web3Backend

--------------------------------------------------------------------------------
-- * Web3 Monad
--------------------------------------------------------------------------------

foreign import data ETH :: Effect

data Web3 b a = Web3 (Eff (eth :: ETH, exception :: EXCEPTION) a)

unWeb3 :: forall b a . Web3 b a -> Eff (eth :: ETH, exception :: EXCEPTION) a
unWeb3 (Web3 action) = action

instance functorWeb3 :: Functor (Web3 p) where
  map f (Web3 m) = Web3 (map f m)

instance applyWeb3 :: Apply (Web3 p) where
  apply (Web3 f) (Web3 m) = Web3 (apply f m)

instance applicativeWeb3 :: Applicative (Web3 p) where
  pure = Web3 <<< pure

instance bindWeb3 :: Bind (Web3 p) where
  bind (Web3 m) f = Web3 (m >>= (unWeb3 <<< f))

instance monadWeb3 :: Monad (Web3 m)

instance effWeb3 :: MonadEff (eth :: ETH, exception :: EXCEPTION) (Web3 b) where
  liftEff = Web3

----------------------------------------------------------------------------------
---- * Eth
----------------------------------------------------------------------------------

-- | Get the balance of an address with respect to a 'BlockId'
getBalance :: forall b .
              Backend b
           => Address
           -> BlockId
           -> Web3 b BigNumber
getBalance addr bid = do
  web3 <- web3Object
  liftEff $ runEffFn2 (_getBalance web3) addr (show bid)

foreign import _getBalance :: forall eff . Web3Backend -> EffFn2 (eth :: ETH | eff) Address String BigNumber

data Block
  = Block { difficulty :: BigNumber
          , extraData :: HexString
          , gasLimit :: BigNumber
          , gasUsed :: BigNumber
          , hash :: HexString
          , logsBloom :: HexString
          , miner :: HexString
          , mixHash :: HexString
          , nonce :: HexString
          , number :: Int
          , parentHash :: HexString
          , receiptsRoot :: HexString
          , sha3Uncles :: HexString
          , size :: Int
          , stateRoot :: HexString
          , timestamp :: BigNumber
          , totalDifficulty :: BigNumber
          , transactions :: Array HexString
          , transactionsRoot :: HexString
          , uncles :: Array HexString
          }

derive instance genericBlock :: Generic Block _

instance showBlock :: Show Block where
  show = genericShow

instance decodeBlock :: Decode Block where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

-- | Get a 'Block' object with respect to a 'BlockId'
getBlock :: forall b .
            Backend b
         => BlockId
         -> Web3 b Block
getBlock bid = do
  web3 <- web3Object
  block <- liftEff $ runEffFn1 (_getBlock web3) (show bid)
  case runExcept <<< decode $ block of
    Left e -> liftEff <<< throw <<< show $ e
    Right res -> pure res

foreign import _getBlock :: forall eff . Web3Backend -> EffFn1 (eth :: ETH | eff) String Foreign

data Transaction =
  Transaction { hash :: HexString
              , nonce :: BigNumber
              , blockHash :: HexString
              , blockNumber :: Int
              , transactionIndex :: Int
              , from :: Address
              , to :: NullOrUndefined Address
              , value :: BigNumber
              , gas :: BigNumber
              , gasPrice :: BigNumber
              , input :: HexString
              }

derive instance genericTransaction :: Generic Transaction _

instance showTransaction :: Show Transaction where
  show = genericShow

instance decodeTransaction :: Decode Transaction where
  decode x = genericDecode (defaultOptions { unwrapSingleConstructors = true }) x

-- | Retrieve a 'Transaction' by its hash.
getTransaction :: forall b .
                  Backend b
               => HexString
               -> Web3 b Transaction
getTransaction txHash = do
  web3 <- web3Object
  res <- liftEff $ runEffFn1 (_getTransaction web3) txHash
  case runExcept <<< decode $ res of
    Left e -> liftEff <<< throw <<< show $ e
    Right tx -> pure tx

foreign import _getTransaction :: forall eff . Web3Backend -> EffFn1 (eth :: ETH | eff) HexString Foreign

-- | Check the connection status of the 'Web3Backend' object
isConnected :: forall b .
               Backend b
            => Web3 b Boolean
isConnected = do
  web3 <- web3Object
  liftEff $ runEffFn1 _isConnected web3

foreign import _isConnected :: forall eff . EffFn1 (eth :: ETH | eff) Web3Backend Boolean

data TransactionOptions =
  TransactionOptions { from :: NullOrUndefined Address
                     , to :: NullOrUndefined Address
                     , value :: NullOrUndefined BigNumber
                     , gas :: NullOrUndefined BigNumber
                     , gasPrice :: NullOrUndefined BigNumber
                     , data :: NullOrUndefined HexString
                     , nonce :: NullOrUndefined Int
                     }

derive instance genericTransactionOptions :: Generic TransactionOptions _

instance showTransactionOptions :: Show TransactionOptions where
  show = genericShow

instance encodeTransactionOptions :: Encode TransactionOptions where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })

defaultTransactionOptions :: TransactionOptions
defaultTransactionOptions =
  TransactionOptions { from : NullOrUndefined Nothing
                     , to : NullOrUndefined Nothing
                     , value : NullOrUndefined Nothing
                     , gas : NullOrUndefined Nothing
                     , gasPrice : NullOrUndefined Nothing
                     , data : NullOrUndefined Nothing
                     , nonce : NullOrUndefined Nothing
                     }

setFrom :: TransactionOptions -> Address -> TransactionOptions
setFrom (TransactionOptions txOptions) f =
  TransactionOptions txOptions {from = NullOrUndefined (Just f)}

type ABI = Json

foreign import data Contract :: Type

newtype ContractInstance a = ContractInstance Contract

foreign import _contract :: Web3Backend -> ABI -> Contract

foreign import _getContractInstance :: forall a . Contract -> Address -> ContractInstance a

newtype MethodName = MethodName String

foreign import _callMethod :: forall eff a .
                              ContractInstance a
                           -> MethodName
                           -> EffFn1 (eth :: ETH | eff) (Array Foreign) Foreign

callMethod :: forall a b .
              Remote b
           => ContractInstance a
           -> MethodName
           -> b
callMethod con meth = _remote $ runEffFn1 (_callMethod con meth)

class Remote a where
  _remote :: (Array Foreign -> Eff (eth :: ETH, exception :: EXCEPTION) Foreign) -> a

instance remoteBase :: Decode a => Remote (Web3 b a) where
  _remote f = do
    res <- Web3 $ f mempty
    case runExcept <<< decode $ res of
      Left e -> Web3 <<< throw <<< show $ e
      Right r -> pure r

instance remoteInductive :: (Encode a, Remote b) => Remote (a -> b) where
  _remote f x = _remote $ \args -> f (cons (encode x) args)
