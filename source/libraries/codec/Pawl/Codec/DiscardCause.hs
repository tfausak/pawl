module Pawl.Codec.DiscardCause where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.DiscardCause as DiscardCause

toJson :: DiscardCause.DiscardCause -> Value.Value
toJson c = Common.nullary $ case c of
  DiscardCause.Ordinary -> "Ordinary"
  DiscardCause.ToPayCyclingCost -> "ToPayCyclingCost"

fromJson :: Value.Value -> Either Text.Text DiscardCause.DiscardCause
fromJson =
  Common.decodeNullary
    "DiscardCause"
    [ ("Ordinary", DiscardCause.Ordinary),
      ("ToPayCyclingCost", DiscardCause.ToPayCyclingCost)
    ]
