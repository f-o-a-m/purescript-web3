module Web3Spec.Encoding.SimpleSpec (spec) where

import Prelude
import Effect.Aff (error, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(Right), either)
import Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Network.Ethereum.Web3.Types (BigNumber, Block, FalseOrObject(..), HexString, SyncStatus(..), fromInt, mkHexString)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readJSON')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "encoding-spec" do
    falseOrObjectTests
    blockTests

falseOrObjectTests :: Spec Unit
falseOrObjectTests =
  describe "FalseOrObject tests" do
    it "can decode FalseOrObject instances that are false" do
      let
        decodedFalse = (runExcept $ readJSON' "false") :: (Either (NonEmptyList ForeignError) (FalseOrObject SyncStatus))
      decodedFalse `shouldEqual` (Right $ FalseOrObject Nothing)
    it "can decode FalseOrObject instances that are objects" do
      let
        decodedObj = runExcept $ readJSON' "{ \"startingBlock\": \"0x0\", \"currentBlock\": \"0x1\", \"highestBlock\": \"0x2\" }"
      decodedObj `shouldEqual` (Right $ FalseOrObject $ Just $ SyncStatus { startingBlock: fromInt 0, currentBlock: fromInt 1, highestBlock: fromInt 2 })

blockTests :: Spec Unit
blockTests =
  describe "Block decoding tests" do
    it "can decode normal blocks" do
      let
        (decodedBlockE :: Either (NonEmptyList ForeignError) Block) = runExcept $ readJSON' blockPlaintext
      dBlock <- unwrap <$> either (throwError <<< error <<< show) pure decodedBlockE
      dBlock.nonce `shouldEqual` (Just $ upToHex "0x0000000000000000")
      dBlock.hash `shouldEqual` (Just $ upToHex "0x093ff26b85b5e3ac3e331f3d766a81990be76ec8ac79f62a81e30faa642dc26f")
      dBlock.timestamp `shouldEqual` fromInt 1507570522
  where
  -- this is block 1 on Eth mainnet
  blockPlaintext = "{\"difficulty\":\"0x1\",\"extraData\":\"0x0000000000000000000000000000000000000000000000000000000000000000759e3fae48d5abad53ab446f31ab3ae1531f2e4c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"gasLimit\":\"0x8000000\",\"gasUsed\":\"0x0\",\"hash\":\"0x093ff26b85b5e3ac3e331f3d766a81990be76ec8ac79f62a81e30faa642dc26f\",\"logsBloom\":\"0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"miner\":\"0x0000000000000000000000000000000000000000\",\"mixHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"nonce\":\"0x0000000000000000\",\"number\":\"0x0\",\"parentHash\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"receiptsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"sha3Uncles\":\"0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347\",\"size\":\"0x273\",\"stateRoot\":\"0xd3811ce828cfc6b07dbedfe073e1ef7e50bda2dac61a901e995c0f460a625cdd\",\"timestamp\":\"0x59dbb35a\",\"totalDifficulty\":\"0x1\",\"transactions\":[],\"transactionsRoot\":\"0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421\",\"uncles\":[]}"

  upToHex = unsafePartial fromJust <<< mkHexString

newtype KovanBlock = KovanBlock
  { difficulty :: BigNumber
  , extraData :: HexString
  , gasLimit :: BigNumber
  , gasUsed :: BigNumber
  , hash :: Maybe HexString
  , logsBloom :: Maybe HexString
  , author :: HexString
  , sealFields :: Array HexString
  , number :: Maybe BigNumber
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
