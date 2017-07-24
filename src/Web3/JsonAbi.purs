module Web3.JsonAbi where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

data FunctionArg =
  FunctionArg { name :: String
              , type :: String
              }

derive instance genericFunctionArg :: Generic FunctionArg _

instance showFunctionArg :: Show FunctionArg where
  show = genericShow

-- | Elementrary contract interface item
data Function =
  Function { name      :: String
           , inputs    :: Array FunctionArg
           }

--derive instance genericDeclaration :: Generic Declaration _
--
--instance showDeclaration :: Show Declaration where
--  show = genericShow
--
--newtype ContractABI = ContractABI { unABI :: Array Declaration }
--
--derive instance genericContractABI :: Generic ContractABI _
--
--instance showContractABI :: Show ContractABI where
--  show = genericShow
