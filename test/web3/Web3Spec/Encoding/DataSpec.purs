module Web3Spec.Encoding.DataSpec (spec, approve) where

import Prelude

import Data.Functor.Tagged (Tagged, tagged)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.Keccak256 (toSelector)
import Network.Ethereum.Core.Signatures as Address
import Network.Ethereum.Web3.Contract (sendTx, mkDataField)
import Network.Ethereum.Web3.Solidity (Tuple2, UIntN)
import Network.Ethereum.Web3.Solidity.AbiEncoding (toDataBuilder)
import Network.Ethereum.Web3.Solidity.Generic (genericFromRecordFields)
import Network.Ethereum.Web3.Solidity.UInt as UIntN
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3)
import Test.QuickCheck (quickCheckGen, (===))
import Test.Spec (Spec, describe, it)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "data maker" do
    it "can make the approval data" $ liftEffect do
      quickCheckGen do
        args <- { _spender: _, _value: _ } <$> Address.generator <*> UIntN.generator (Proxy @256)
        let
          approvalD = mkDataField (Proxy :: Proxy ApproveFn) args

          sel = toSelector "approve(address,uint256)"

          fullDat = sel <> toDataBuilder args._spender <> toDataBuilder args._value
        pure $ approvalD === fullDat

type ApproveFn = Tagged "approve(address,uint256)" (Tuple2 (Tagged "_spender" Address) (Tagged "_value" (UIntN 256)))

approve :: TransactionOptions NoPay -> { _spender :: Address, _value :: (UIntN 256) } -> Web3 HexString
approve txOpts r = sendTx txOpts (tagged (genericFromRecordFields r) :: ApproveFn)
