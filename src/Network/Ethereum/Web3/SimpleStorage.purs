module Network.Ethereum.Web3.SimpleStorage where


import Prelude
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (fail)

import Network.Ethereum.Web3.Types (HexString(..), Address(..), Web3M, BigNumber, CallMode(..))
import Network.Ethereum.Web3.Contract (sendTx, call)
import Network.Ethereum.Web3.Encoding.AbiEncoding (class ABIEncoding, toDataBuilder)

ssAddress :: Address
ssAddress = Address <<< HexString $ "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = Address <<< HexString $ "44cba02c089789b3299069c93832b7a8b8723b3e"

data Set = Set Int

instance abiEncodingSet :: ABIEncoding Set where
  toDataBuilder (Set n) = HexString "60fe47b1" <> toDataBuilder n
  fromDataParser = fail "No function parser"

data Get = Get

instance abitEncodingGet :: ABIEncoding Get where
  toDataBuilder _ = HexString "6d4ce63c"
  fromDataParser = fail "No function parser"

set :: Int -> Web3M () HexString
set n = sendTx (Just ssAddress) adminAddress zero (Set n)

get :: Web3M () BigNumber
get = call ssAddress Nothing Latest Get

