module Web3.Utils.Utils where

import Prelude
import Data.Array (unsafeIndex)
import Data.ByteString (toString, fromString)
import Data.String (Pattern(..), split)
import Node.Encoding(Encoding(Hex, UTF8, ASCII))
import Partial.Unsafe (unsafePartial)
import Web3.Utils.Sha3 (HexString(..))

data EtherUnit =
    Wei
  | KWei
  | MWei
  | GWei
  | Szabo
  | Finney
  | Ether
  | MEther
  | GEther
  | TEther

toWei :: EtherUnit -> String
toWei eu = case eu of
  Wei    -> "1"
  KWei   -> "1000"
  MWei   -> "1000000"
  GWei   -> "1000000000"
  Szabo  -> "1000000000000"
  Finney -> "1000000000000000"
  Ether  -> "100000000000000000"
  MEther -> "1000000000000000000000"
  GEther -> "1000000000000000000000000"
  TEther -> "1000000000000000000000000000"

-- | Takes a hex string and produces the corresponding UTF8-decoded string.
-- This breaks at the first null octet, following the web3 function 'toUft8'.
-- Since 'split' always returns a nonempty list, this index is actually safe.
toUtf8 :: HexString -> String
toUtf8 (HexString hx) =
  let hx' = unsafePartial $ split (Pattern "00") hx `unsafeIndex` 0
  in flip toString UTF8 $ fromString hx' Hex

-- | Takes a hex string and produces the corresponding ASII string.
toAscii :: HexString -> String
toAscii (HexString hx) = flip toString ASCII $ fromString hx Hex
