module Pawl.Codec.TriggerFrequency where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency

toJson :: TriggerFrequency.TriggerFrequency -> Value.Value
toJson f = Common.nullary $ case f of
  TriggerFrequency.EveryTime -> "EveryTime"
  TriggerFrequency.FirstTimeEachTurn -> "FirstTimeEachTurn"

fromJson :: Value.Value -> Either Text.Text TriggerFrequency.TriggerFrequency
fromJson =
  Common.decodeNullary
    "TriggerFrequency"
    [ ("EveryTime", TriggerFrequency.EveryTime),
      ("FirstTimeEachTurn", TriggerFrequency.FirstTimeEachTurn)
    ]
