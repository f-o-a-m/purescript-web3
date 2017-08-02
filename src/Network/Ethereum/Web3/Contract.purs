module Network.Ethereum.Web3.Contract where

import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.Ethereum.Web3.Encoding (class ABIEncoding, toDataBuilder, fromDataParser)
import Network.Ethereum.Web3.Types (Web3M, ETH, BigNumber, Address, HexString)

class ABIEncoding a <= Method a where
    -- | Send a transaction for given contract 'Address', value and input data
    sendTx :: forall eff .
              Address
           -- ^ Contract address
           -> BigNumber
           -- ^ paymentValue
           -> a
           -- ^ Method data
           -> Web3M (eth :: ETH, exception :: EXCEPTION | eff) HexString
           -- ^ 'Web3' wrapped tx hash

    -- | Constant call given contract 'Address' in mode and given input data
    call :: forall eff b .
            ABIEncoding b
         => Address
         -- ^ Contract address
         -- ^ State mode for constant call (latest or pending)
         -> a
         -- ^ Method data
         -> Web3M (eth :: ETH, exception :: EXCEPTION | eff) b
         -- ^ 'Web3' wrapped result

--_call :: forall a b eff .
--         ABIEncoding a
--      => ABIEncoding b
--      => Address
--      -> a
--      -> Web3 (eth :: ETH, exception :: EXCEPTION | eff) b
--_call to dat = do
--    res <- eth_call (txdata primeAddress)
--    case fromData (T.drop 2 res) of
--        Nothing -> liftIO $ throwIO $ ParserFail $
--            "Unable to parse result on `" ++ T.unpack res
--            ++ "` from `" ++ show to ++ "`"
--        Just x -> return x
