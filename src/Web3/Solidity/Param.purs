module Web3.Solidity.Param
  ( SolidityParam(..)
  , paramValue
  , isDynamic
  , dynamicPart
  , dynamicPartLength
  , withOffset
  , combine
  , offsetAsBytes
  , staticPart
  , encode
  , encodeList
  ) where

import Prelude
import Control.Monad.State (StateT, get, put, evalStateT)
import Data.Identity (Identity(..))
import Data.Foldable (fold, foldl)
import Data.Traversable (traverse)
import Data.List (List)
import Data.List (length) as L
import Data.Maybe (Maybe, fromMaybe, isJust, maybe)
import Data.Monoid (mempty)
import Web3.Utils.Types (HexString, length)
import Web3.Utils.Utils (padLeft)
import Web3.Utils.BigNumber (embed, toTwosComplement, toSignedHexString)

data SolidityParam =
  SolidityParam { value :: HexString
                , offset :: Maybe Int
                }

derive instance eqSolidityParam :: Eq SolidityParam

instance showSolidityParam :: Show SolidityParam where
  show (SolidityParam p) = fold $
    [ "{ value: "
    , show p.value
    , ", offset: "
    , maybe "null" show p.offset
    , " }"
    ]

paramValue :: SolidityParam -> HexString
paramValue (SolidityParam p) = p.value

-- | This method should be called to check if param has dynamic size.
-- If it has, it returns true, otherwise false
isDynamic :: SolidityParam -> Boolean
isDynamic (SolidityParam p) = isJust $ p.offset

-- | This method should be called to get dynamic part of param
dynamicPart :: SolidityParam -> HexString
dynamicPart param@(SolidityParam p) =
  if isDynamic param then p.value else mempty

-- | This method should be used to get length of params's dynamic part
dynamicPartLength :: SolidityParam -> Int
dynamicPartLength = length <<< dynamicPart

-- | This method should be used to create copy of solidity param with different offset
withOffset :: Int -> SolidityParam -> SolidityParam
withOffset i (SolidityParam p) = SolidityParam p { offset = const i <$> p.offset }

-- | This method should be used to combine solidity params together
-- eg. when appending an array
combine :: SolidityParam -> SolidityParam -> HexString
combine (SolidityParam p1) (SolidityParam p2) =
  p1.value <> p2.value

-- | This method should be called to transform offset to bytes
offsetAsBytes :: SolidityParam -> Maybe HexString
offsetAsBytes (SolidityParam p) = do
  os <- p.offset
  let bn = toTwosComplement $ embed os
  pure <<< flip padLeft 64 $ toSignedHexString bn

-- | This method should be called to get static part of param
staticPart :: SolidityParam -> HexString
staticPart param@(SolidityParam p) =
  fromMaybe p.value $ offsetAsBytes param

-- | This method should be called to encode param.
encode :: SolidityParam -> HexString
encode p = staticPart p <> dynamicPart p

-- | This method should be called to encode array of params
encodeList :: List SolidityParam -> HexString
encodeList params =
  let totalOffset = 32 * L.length params
      Identity offsetParams = flip evalStateT totalOffset $
        traverse incrementParam params
  in reduceParams offsetParams

incrementParam :: SolidityParam -> StateT Int Identity SolidityParam
incrementParam p =
  if not <<< isDynamic $ p then pure p else do
    offset <- get
    put $ offset + dynamicPartLength p
    pure $ withOffset offset p

reduceParams :: List SolidityParam -> HexString
reduceParams ps =
  let statics = fold <<< map staticPart $ ps
  in foldl (\hx p -> hx <> dynamicPart p) statics ps
