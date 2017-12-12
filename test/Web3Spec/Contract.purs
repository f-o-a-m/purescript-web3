module Web3Spec.Contract  where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Lens ((.~))
import Data.Machine.Mealy (MealyT, Step(..), runMealy, runMealyT, sink)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, wrap)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (_fromBlock, _toBlock, _topics)
import Network.Ethereum.Web3.Contract (class EventFilter, FilterStreamState, boundedFilterStream, sendTx)
import Network.Ethereum.Web3.Provider (class IsAsyncProvider, httpProvider, runWeb3)
import Network.Ethereum.Web3.Solidity (type (:&), D2, D5, D6, IntN, Tuple1(..), UIntN, intNFromBigNumber)
import Network.Ethereum.Web3.Types (Address, ETH, Ether, HexString, Value, Web3(..), embed, mkAddress, mkHexString, unHex)
import Network.Ethereum.Web3.Types.Types (HexString(..), _address, defaultFilter)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

ssAddress :: Address
ssAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "c29313014a78b440876bac21be369c3047e313e7"

adminAddress :: Address
adminAddress = unsafePartial fromJust $ mkAddress =<< mkHexString "44cba02c089789b3299069c93832b7a8b8723b3e"

type FnSet = Tagged (SProxy "(int256)") (Tuple1 (IntN (D2 :& D5 :& D6)))

data HttpProvider

http :: Proxy HttpProvider
http = Proxy

--------------------------------------------------------------------------------
-- | CountSet
--------------------------------------------------------------------------------

newtype CountSet = CountSet {_count :: (UIntN (D2 :& D5 :& D6))}

derive instance newtypeCountSet :: Newtype CountSet _

instance eventFilterCountSet :: EventFilter CountSet where
        eventFilter _ addr = defaultFilter
                # _address .~ Just addr
                # _topics .~ Just [Just (HexString "a32bc18230dd172221ac5c4821a5f1f1a831f27b1396d244cdd891c58f132435")]
                # _fromBlock .~ Nothing
                # _toBlock .~ Nothing

instance isAsyncHttp :: IsAsyncProvider HttpProvider where
  getAsyncProvider = Web3 <<< liftEff <<< httpProvider $ "http://localhost:8545"

setA :: forall e . IntN (D2 :& D5 :& D6) -> Web3 HttpProvider e HexString
setA n = sendTx (Just ssAddress) adminAddress (zero :: Value Ether) ((tagged <<< Tuple1 $ n) :: FnSet)

runFilterStream :: forall eff . Eff (console :: CONSOLE | eff) Unit
runFilterStream =
    let s = { currentBlock: wrap <<< embed $ 0
            , endingBlock: wrap <<< embed $ 10
            , windowSize: 2
            }
    in go (boundedFilterStream (Proxy :: Proxy CountSet) ssAddress) s
  where
    go machine s = do
      a <- runMealyT machine s
      case a of
        Emit f machine' -> do
          logShow f
          go machine' s
        Halt -> pure unit

simpleStorageSpec :: forall r. Spec (eth :: ETH, console :: CONSOLE | r) Unit
simpleStorageSpec =
  describe "Bounded event handlers" do
    it "can print the lifespan of a filter producing machine" do
      liftEff runFilterStream
      true `shouldEqual` true
