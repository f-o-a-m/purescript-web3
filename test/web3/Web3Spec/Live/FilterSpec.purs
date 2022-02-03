module Web3Spec.Live.FilterSpec (spec) where

import Prelude
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Array ((..), snoc, length, head, sortWith)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap, un)
import Data.Ord.Down (Down(..))
import Data.Traversable (traverse_)
import Data.Lens ((?~), (.~), (^.))
import Effect.Aff (Aff, Fiber, error)
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.AVar as AVar
import Effect.AVar as EAVar
import Network.Ethereum.Web3 (BlockNumber(..), throwWeb3, Filter, Web3Error, Change(..), _fromBlock, _toBlock, eventFilter, EventAction(..), forkWeb3, ChainCursor(..), Provider, UIntN, _from, _to, embed, Address, event')
import Network.Ethereum.Web3.Api as Api
import Network.Ethereum.Web3.Solidity.Sizes (s256, S256)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (SpecT, before, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Web3Spec.Live.Contract.SimpleStorage as SimpleStorage
import Web3Spec.Live.Code.SimpleStorage as SimpleStorageCode
import Web3Spec.Live.Utils (assertWeb3, go, Logger, defaultTestTxOptions, ContractConfig, deployContract, mkUIntN, pollTransactionReceipt, joinWeb3Fork, hangOutTillBlock)

spec :: Provider -> SpecT Aff Unit Aff Unit
spec p =
  let
    env =
      { logger: \s -> ask >>= \logger -> liftAff $ logger s
      }
  in
    go $ spec' p env

type FilterEnv m
  = { logger :: String -> m Unit
    }

{-
NOTE: none of the Futures use Pending, the behavior is currently ill defined

Case [Past,Past] : The filter is starting and ending in the past.
Case [Past, ∞] : The filter starts in the past but continues indefinitely into the future.
Case [Future, ∞] : The fitler starts in the future and continues indefinitely into the future.
Case [Future, Future] : The fitler starts in the future and ends at a later time in the future.
-}
spec' ::
  forall m.
  MonadAff m =>
  Provider ->
  FilterEnv m ->
  SpecT m Unit Aff Unit
spec' provider { logger } = do
  uIntV <- liftEffect $ EAVar.new 1
  let
    uIntsGen = mkUIntsGen uIntV
  describe "Filters"
    $ parallel do
        before (deployUniqueSimpleStorage provider logger)
          $ it "Case [Past, Past]" \simpleStorageCfg -> do
              let
                { simpleStorageAddress, setter } = simpleStorageCfg

                filter = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
              values <- uIntsGen 3
              logger $ "Searching for values " <> show values
              fiber <- monitorUntil provider logger filter (_ == aMax values) defaultFilterOpts
              start <- assertWeb3 provider Api.eth_blockNumber
              traverse_ setter values
              { endingBlockV } <- joinWeb3Fork fiber
              end <- liftAff $ AVar.take endingBlockV
              let
                pastFilter =
                  eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                    # _fromBlock
                    .~ BN start
                    # _toBlock
                    .~ BN end
              fiber' <- monitorUntil provider logger pastFilter (const false) defaultFilterOpts
              { foundValuesV } <- joinWeb3Fork fiber'
              foundValues <- liftAff $ AVar.take foundValuesV
              liftAff $ foundValues `shouldEqual` values
        before (deployUniqueSimpleStorage provider logger)
          $ do
              it "Case [Past, ∞] No Trail" \simpleStorageCfg -> do
                fromPastToFutureTrailingBy uIntsGen simpleStorageCfg provider logger defaultFilterOpts
              it "Case [Past, ∞] With Trail" \simpleStorageCfg -> do
                fromPastToFutureTrailingBy uIntsGen simpleStorageCfg provider logger { trailBy: 3, windowSize: 2 }
        before (deployUniqueSimpleStorage provider logger)
          $ it "Case [Future, ∞]" \simpleStorageCfg -> do
              let
                { simpleStorageAddress, setter } = simpleStorageCfg
              values <- uIntsGen 3
              logger $ "Searching for values " <> show values
              now <- assertWeb3 provider Api.eth_blockNumber
              let
                later = wrap $ unwrap now + embed 3

                filter =
                  eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                    # _fromBlock
                    .~ BN later
                    # _toBlock
                    .~ Latest
              fiber <- monitorUntil provider logger filter (_ == aMax values) defaultFilterOpts
              hangOutTillBlock provider logger later
              traverse_ setter values
              { foundValuesV } <- joinWeb3Fork fiber
              foundValues <- liftAff $ AVar.take foundValuesV
              liftAff $ foundValues `shouldEqual` values
        before (deployUniqueSimpleStorage provider logger)
          $ it "Case [Future, Future]" \simpleStorageCfg -> do
              let
                { simpleStorageAddress, setter } = simpleStorageCfg
              values <- uIntsGen 3
              logger $ "Searching for values " <> show values
              let
                nValues = length values
              now <- assertWeb3 provider Api.eth_blockNumber
              let
                later = wrap $ unwrap now + embed 3

                -- NOTE: This isn't that clean, but 2 blocks per set should be enough time
                latest = wrap $ unwrap later + embed (2 * nValues)

                filter =
                  eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
                    # _fromBlock
                    .~ BN later
                    # _toBlock
                    .~ BN latest
              fiber <- monitorUntil provider logger filter (_ == aMax values) defaultFilterOpts
              hangOutTillBlock provider logger later
              traverse_ setter values
              { foundValuesV } <- joinWeb3Fork fiber
              foundValues <- liftAff $ AVar.take foundValuesV
              liftAff $ foundValues `shouldEqual` values

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
type FilterOpts
  = { trailBy :: Int
    , windowSize :: Int
    }

defaultFilterOpts :: FilterOpts
defaultFilterOpts = { trailBy: 0, windowSize: 0 }

fromPastToFutureTrailingBy ::
  forall m.
  MonadAff m =>
  (Int -> m (Array (UIntN S256))) ->
  SimpleStorageCfg m ->
  Provider ->
  Logger m ->
  FilterOpts ->
  m Unit
fromPastToFutureTrailingBy uIntsGen simpleStorageCfg provider logger opts = do
  let
    { simpleStorageAddress, setter } = simpleStorageCfg

    filter1 = eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
  firstValues <- uIntsGen 7
  secondValues <- uIntsGen 7
  let
    allValues = firstValues <> secondValues
  logger $ "Searching for values " <> show allValues
  fiber1 <- monitorUntil provider logger filter1 (_ == aMax firstValues) defaultFilterOpts
  start <- assertWeb3 provider Api.eth_blockNumber
  traverse_ setter firstValues
  _ <- joinWeb3Fork fiber1
  let
    filter2 =
      eventFilter (Proxy :: Proxy SimpleStorage.CountSet) simpleStorageAddress
        # _fromBlock
        .~ BN start
        # _toBlock
        .~ Latest
  fiber2 <- monitorUntil provider logger filter2 (_ == aMax secondValues) opts
  traverse_ setter secondValues
  { foundValuesV, reachedTargetTrailByV } <- joinWeb3Fork fiber2
  foundValues <- liftAff $ AVar.take foundValuesV
  liftAff $ foundValues `shouldEqual` allValues
  when (opts.trailBy > 0) do
    reachedTargetTrailBy <- liftAff $ AVar.take reachedTargetTrailByV
    logger $ "Reached 'chainHead - trailBy' : " <> show reachedTargetTrailBy
    liftAff $ reachedTargetTrailBy `shouldEqual` true

monitorUntil ::
  forall m.
  MonadAff m =>
  Provider ->
  Logger m ->
  Filter SimpleStorage.CountSet ->
  (UIntN S256 -> Boolean) ->
  FilterOpts ->
  m
    ( Fiber
        ( Either Web3Error
            { endingBlockV :: AVar.AVar BlockNumber
            , foundValuesV :: AVar.AVar (Array (UIntN S256))
            , reachedTargetTrailByV :: AVar.AVar Boolean
            }
        )
    )
monitorUntil provider logger filter p opts = do
  endingBlockV <- liftAff AVar.empty
  foundValuesV <- liftAff $ AVar.new []
  reachedTargetTrailByV <- liftAff $ AVar.new false
  logger $ "Creating filter with fromBlock="
    <> show (filter ^. _fromBlock)
    <> " toBlock="
    <> show (filter ^. _toBlock)
  let
    handler (SimpleStorage.CountSet { _count }) = do
      Change c <- ask
      chainHead <- lift Api.eth_blockNumber
      when (un BlockNumber chainHead - un BlockNumber c.blockNumber < embed opts.trailBy)
        $ lift
        $ throwWeb3
        $ error "Exceded max trailBy"
      when (un BlockNumber chainHead - un BlockNumber c.blockNumber == embed opts.trailBy) do
        _ <- liftAff $ AVar.take reachedTargetTrailByV
        liftAff $ AVar.put true reachedTargetTrailByV
      foundSoFar <- liftAff $ AVar.take foundValuesV
      liftAff $ AVar.put (foundSoFar `snoc` _count) foundValuesV
      let
        shouldTerminate = p _count
      if shouldTerminate then do
        liftAff $ AVar.put c.blockNumber endingBlockV
        pure TerminateEvent
      else
        pure ContinueEvent
  liftAff $ forkWeb3 provider
    $ do
        _ <- event' { ev: filter } { ev: handler } opts
        pure { endingBlockV, foundValuesV, reachedTargetTrailByV }

type SimpleStorageCfg m
  = { simpleStorageAddress :: Address
    , setter :: UIntN S256 -> m Unit
    }

deployUniqueSimpleStorage ::
  forall m.
  MonadAff m =>
  Provider ->
  Logger m ->
  m (SimpleStorageCfg m)
deployUniqueSimpleStorage provider logger = do
  contractConfig <-
    deployContract provider logger "SimpleStorage"
      $ \txOpts ->
          SimpleStorage.constructor txOpts SimpleStorageCode.deployBytecode
  pure
    { simpleStorageAddress: contractConfig.contractAddress
    , setter: mkSetter contractConfig provider logger
    }

mkSetter ::
  forall m.
  MonadAff m =>
  ContractConfig ->
  Provider ->
  Logger m ->
  UIntN S256 ->
  m Unit
mkSetter { contractAddress, userAddress } provider logger _count = do
  let
    txOptions =
      defaultTestTxOptions # _from ?~ userAddress
        # _to
        ?~ contractAddress
  logger $ "Setting count to " <> show _count
  txHash <- assertWeb3 provider $ SimpleStorage.setCount txOptions { _count }
  _ <- pollTransactionReceipt provider txHash
  pure unit

mkUIntsGen ::
  forall m.
  MonadAff m =>
  AVar.AVar Int ->
  Int ->
  m (Array (UIntN S256))
mkUIntsGen uintV n =
  liftAff do
    firstAvailable <- AVar.take uintV
    let
      nextVal = firstAvailable + n

      res = firstAvailable .. (nextVal - 1)
    AVar.put nextVal uintV
    pure $ map (mkUIntN s256) res

aMax :: forall a. Ord a => Array a -> a
aMax as = case head $ sortWith Down as of
  Nothing -> unsafeCrashWith "Can't take the max of an empty array"
  Just a -> a
