module Pawl.Codec.SpecialAction where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.SpecialAction as SpecialAction

toJson :: SpecialAction.SpecialAction -> Value.Value
toJson a = Common.nullary $ case a of
  SpecialAction.DiscardThisAnyTime -> "DiscardThisAnyTime"

fromJson :: Value.Value -> Either Text.Text SpecialAction.SpecialAction
fromJson =
  Common.decodeNullary
    "SpecialAction"
    [("DiscardThisAnyTime", SpecialAction.DiscardThisAnyTime)]
