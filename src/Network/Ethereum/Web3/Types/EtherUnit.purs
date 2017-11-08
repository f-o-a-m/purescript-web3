module Network.Ethereum.Web3.Types.EtherUnit
  ( class EtherUnit, fromWei, toWei
  , class EtherUnitSpec
  , divider
  , name
  , convert
  , Wei
  , Babbage
  , Lovelace
  , Shannon
  , Szabo
  , Finney
  , Ether
  , KEther
  , Value
  , mkValue
  , noPay
  ) where

import Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Maybe (fromJust)
import Network.Ethereum.Web3.Types.BigNumber (BigNumber, decimal, floorBigNumber, parseBigNumber)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


-- | Ether value in denomination `a`
newtype Value a = Value BigNumber

derive newtype instance eqValue :: Eq (Value a)

derive newtype instance showValue :: Show (Value a)

derive newtype instance encodeValue ::  Encode (Value a)

derive newtype instance decodeValue ::  Decode (Value a)

unValue :: forall a . Value a -> BigNumber
unValue (Value a) = a

-- | Useful for converting to and from the base denomination `Wei`
class EtherUnit a where
    fromWei :: BigNumber -> a
    toWei :: a -> BigNumber

-- | Convert between two denominations
convert :: forall a b . EtherUnit a => EtherUnit b => a -> b
convert = fromWei <<< toWei

class EtherUnitSpec a where
    divider :: Proxy a -> BigNumber
    name    :: Proxy a -> String

-- | Convert a big number into value, first using `floor` function to take the integer part
mkValue :: forall a . EtherUnitSpec a => BigNumber -> Value a
mkValue = modify res <<< floorBigNumber <<< (mul (divider res))
  where res :: EtherUnitSpec a => Proxy a
        res = Proxy
        modify :: Proxy a -> BigNumber -> Value a
        modify _ = Value

instance unitEtherUnitSpec :: EtherUnitSpec a => EtherUnit (Value a) where
    fromWei = Value
    toWei = unValue

instance semiringEtherUnitSpec :: EtherUnitSpec a => Semiring (Value a) where
   add a b = Value (unValue a `add` unValue b)
   mul a b = Value (unValue a `mul` unValue b)
   zero = Value zero
   one = Value one

instance ringEtherUnitSpec :: EtherUnitSpec a => Ring (Value a) where
   sub a b = Value (unValue a `sub` unValue b)

data Wei

instance unitSpecWei :: EtherUnitSpec Wei where
    divider = const $ unsafeConvert "1"
    name    = const "wei"

-- | Babbage unit type
data Babbage

instance unitSpecB :: EtherUnitSpec Babbage where
    divider = const $ unsafeConvert "1000"
    name    = const "babbage"

-- | Lovelace unit type
data Lovelace

instance unitSpecL :: EtherUnitSpec Lovelace where
    divider = const $ unsafeConvert "1000000"
    name    = const "lovelace"

-- | Shannon unit type
data Shannon

instance unitSpecS :: EtherUnitSpec Shannon where
    divider = const $ unsafeConvert "1000000000"
    name    = const "shannon"

-- | Szabo unit type
data Szabo

instance unitSpecSz :: EtherUnitSpec Szabo where
    divider = const $ unsafeConvert "1000000000000"
    name    = const "szabo"

-- | Finney unit type
data Finney

instance unitSpecF :: EtherUnitSpec Finney where
    divider = const $ unsafeConvert "1000000000000000"
    name    = const "finney"

-- | Ether unit type
data Ether

instance unitSpecE :: EtherUnitSpec Ether where
    divider = const $ unsafeConvert "1000000000000000000"
    name    = const "ether"

-- | KEther unit type
data KEther

instance unitSpecKE :: EtherUnitSpec KEther where
    divider = const $ unsafeConvert $ "1000000000000000000000"
    name    = const "kether"

noPay :: Value Wei
noPay = Value zero

unsafeConvert :: String -> BigNumber
unsafeConvert a = unsafePartial $ fromJust <<< parseBigNumber decimal $ a
