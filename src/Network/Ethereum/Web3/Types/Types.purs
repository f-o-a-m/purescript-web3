module Network.Ethereum.Web3.Types.Types where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Eff.Exception (Error, EXCEPTION, throwException)
import Data.Monoid (class Monoid)
import Data.Array (all ,elem)
import Data.ByteString (ByteString, Encoding(Hex))
import Data.ByteString as BS
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray, stripPrefix,  Pattern(..))
import Data.String (length) as S

import Network.Ethereum.Web3.Types.BigNumber (BigNumber)

--------------------------------------------------------------------------------
-- * Signed Values
--------------------------------------------------------------------------------

data Sign = Pos | Neg

derive instance eqSign :: Eq Sign

data Signed a = Signed Sign a

instance showSigned :: Show a => Show (Signed a) where
  show (Signed s a) = show s' <> show a
    where
      s' = case s of
        Pos -> ""
        Neg -> "-"

instance eqSigned :: Eq a => Eq (Signed a) where
  eq (Signed s a) (Signed s' a') = (s `eq` s') && (a `eq` a')

instance mapSigned :: Functor Signed where
  map f (Signed s a) = Signed s (f a)

unSigned :: forall a . Signed a -> a
unSigned (Signed _ a) = a

asSigned :: forall a . a -> Signed a
asSigned a = Signed Pos a

--------------------------------------------------------------------------------
-- * HexString
--------------------------------------------------------------------------------

newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString hx) = "0x" <> hx

derive newtype instance hexStringEq :: Eq HexString

derive newtype instance semigpStringEq :: Semigroup HexString

derive newtype instance monoidStringEq :: Monoid HexString

instance decodeHexString :: Decode HexString where
  decode s = do
    str <- decode s
    case stripPrefix (Pattern "0x") str of
      Nothing -> pure <<< HexString $ str
      Just res -> pure <<< HexString $ res

instance encodeHexString :: Encode HexString where
  encode = encode <<< append "0x" <<< unHex

parseHexString :: String -> Maybe HexString
parseHexString s =
    let res = all go <<< toCharArray $ s
    in if res then Just <<< HexString $ s else Nothing
  where
    go :: Char -> Boolean
    go c = c `elem` ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']

unHex :: HexString -> String
unHex (HexString hx) = hx

pack :: HexString -> ByteString
pack (HexString hx) = BS.fromString hx Hex

length :: HexString -> Int
length (HexString hx) = S.length hx

--------------------------------------------------------------------------------
-- * Addresses
--------------------------------------------------------------------------------

newtype Address = Address HexString

derive newtype instance addressShow :: Show Address

derive newtype instance addressEq :: Eq Address

derive newtype instance decodeAddress :: Decode Address

derive newtype instance encodeAddress :: Encode Address

--------------------------------------------------------------------------------
-- * Block
--------------------------------------------------------------------------------

data CallMode =
    Latest
  | Pending
  | Earliest
  | BlockNumber BigNumber

instance encodeCallMode :: Encode CallMode where
  encode cm = case cm of
    Latest -> encode "latest"
    Pending -> encode "pending"
    Earliest -> encode "earliest"
    BlockNumber n -> encode n

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
          , number :: BigNumber
          , parentHash :: HexString
          , receiptsRoot :: HexString
          , sha3Uncles :: HexString
          , size :: BigNumber
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

--------------------------------------------------------------------------------
-- * Transaction
--------------------------------------------------------------------------------

data Transaction =
  Transaction { hash :: HexString
              , nonce :: BigNumber
              , blockHash :: HexString
              , blockNumber :: BigNumber
              , transactionIndex :: BigNumber
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

--------------------------------------------------------------------------------
-- * TransactionOptions
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- | Web3M
--------------------------------------------------------------------------------

-- | Synchronous Web3 Actions

foreign import data ETH :: Effect

newtype Web3M e a = Web3M (Eff (eth :: ETH , exception :: EXCEPTION | e) a)

derive newtype instance functorWeb3M :: Functor (Web3M e)

derive newtype instance applyWeb3M :: Apply (Web3M e)

derive newtype instance applicativeWeb3M :: Applicative (Web3M e)

derive newtype instance bindWeb3M :: Bind (Web3M e)

derive newtype instance monadWeb3M :: Monad (Web3M e)

derive newtype instance monadEffWeb3M :: MonadEff (eth :: ETH, exception :: EXCEPTION | e) (Web3M e)

instance monadThrowWeb3M :: MonadThrow Error (Web3M e) where
    throwError = Web3M <<< throwException

unWeb3M :: forall eff a . Web3M eff a -> Eff (eth :: ETH , exception :: EXCEPTION | eff) a
unWeb3M (Web3M action) = action

-- | Asynchronous Web3 Actions

newtype Web3MA e a = Web3MA (Aff (eth :: ETH | e) a)

derive newtype instance functorWeb3MA :: Functor (Web3MA e)

derive newtype instance applyWeb3MA :: Apply (Web3MA e)

derive newtype instance applicativeWeb3MA :: Applicative (Web3MA e)

derive newtype instance bindWeb3MA :: Bind (Web3MA e)

derive newtype instance monadWeb3MA :: Monad (Web3MA e)

derive newtype instance monadEffWeb3MA :: MonadEff (eth :: ETH | e) (Web3MA e)

derive newtype instance monadThrowWeb3MA :: MonadThrow Error (Web3MA e)

unWeb3MA :: forall eff a . Web3MA eff a -> Aff (eth :: ETH | eff) a
unWeb3MA (Web3MA action) = action

--------------------------------------------------------------------------------
-- * Contract Interface and Event Description
--------------------------------------------------------------------------------

data AbiElement = FunctionType | Event

type FunctionType = { type_ :: FunctionClass
                    , name :: String
                    , inputs :: List FunctionInputs
                    , constant :: Boolean
                    , payable :: Boolean
                    }

data FunctionClass =
    Function
  | Constructor
  | Fallback

type FunctionInputs = { name :: String
                      , type_ :: String
                      }

type Event = { name :: String
             , inputs :: List EventInputs
             , anonymous :: Boolean
             }

type EventInputs = { name :: String
                   , type_ :: String
                   , indexed :: Boolean
                   }
