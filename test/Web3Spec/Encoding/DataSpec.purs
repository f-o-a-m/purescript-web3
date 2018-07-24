module Web3Spec.Encoding.DataSpec where

-- There is no test here, this just needs to compile

import Network.Ethereum.Web3.Solidity (UIntN, Tuple2)
import Network.Ethereum.Web3.Solidity.Size (type (:&), D2, D5, D6, DOne)
import Network.Ethereum.Web3.Types (Address, HexString, TransactionOptions, NoPay, Web3)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3.Contract (sendTx)
import Network.Ethereum.Web3.Solidity.Generic (genericFromRecordFields)

type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "_spender") Address) (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))



approve :: forall e. TransactionOptions NoPay -> { _spender :: Address, _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
approve txOpts r = sendTx txOpts (tagged (genericFromRecordFields r) :: ApproveFn)
