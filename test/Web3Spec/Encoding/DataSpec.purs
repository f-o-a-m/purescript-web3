module Web3Spec.Encoding.DataSpec (dataMakerSpec) where

-- There is no test here, this just needs to compile

import Prelude
import Data.Maybe (fromJust)
import Network.Ethereum.Web3.Solidity (UIntN, Tuple2, uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Size (type (:&), D2, D5, D6, DOne)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types (Address, HexString, TransactionOptions, NoPay, Web3, mkHexString, mkAddress)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Network.Ethereum.Web3.Contract (sendTx)
import Network.Ethereum.Core.Keccak256 (toSelector)
import Network.Ethereum.Web3.Solidity.Generic (class RecordFieldsIso, class GenericABIEncode, genericFromRecordFields, genericABIEncode)
import Data.Generic.Rep (class Generic, Constructor)
import Type.Proxy (Proxy(..))
import Partial.Unsafe (unsafePartial)

import Network.Ethereum.Web3.Solidity.AbiEncoding (toDataBuilder)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


dataMakerSpec :: forall r . Spec r Unit
dataMakerSpec =
    describe "data maker" do

      it "can make the approval data" do
        let addr = unsafePartial fromJust $ (mkAddress =<< mkHexString "78534a937a855e15be172de35f2211626f92f8ec")
            val = unsafePartial fromJust $ uIntNFromBigNumber s256 one
            approvalD = dataMaker (Proxy :: Proxy ApproveFn) {_spender: addr, _value: val}
            sel = toSelector "approve(address,uint256)"
            fullDat = sel <> toDataBuilder addr <> toDataBuilder val
        approvalD `shouldEqual` fullDat

type ApproveFn = Tagged (SProxy "approve(address,uint256)") (Tuple2 (Tagged (SProxy "_spender") Address) (Tagged (SProxy "_value") (UIntN (D2 :& D5 :& DOne D6))))


approve :: forall e. TransactionOptions NoPay -> { _spender :: Address, _value :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 e HexString
approve txOpts r = sendTx txOpts (tagged (genericFromRecordFields r) :: ApproveFn)


dataMaker
  :: forall selector a name args fields l.
     IsSymbol selector
  => Generic a (Constructor name args)
  -- ^^ a ~ TupleN (Tagged s1 a1) (Tagged s2 a2) ...
  => RecordFieldsIso args fields l
  => GenericABIEncode (Constructor name args)
  => Proxy (Tagged (SProxy selector) a)
  -> Record fields
  -> HexString
dataMaker (Proxy :: Proxy (Tagged (SProxy selector) a)) r =
  let sig = reflectSymbol (SProxy :: SProxy selector)
      sel = toSelector sig
      args = genericFromRecordFields r :: a
  in sel <> (genericABIEncode args)
