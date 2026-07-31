-- | The @TriggerFrequency ⇆ Json@ codec (#481).
module Pawl.Codec.TriggerFrequency where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency

triggerFrequencyToJson :: TriggerFrequency.TriggerFrequency -> Value
triggerFrequencyToJson f = Json.nullary . Text.pack $ case f of
  TriggerFrequency.EveryTime -> "EveryTime"
  TriggerFrequency.FirstTimeEachTurn -> "FirstTimeEachTurn"

jsonToTriggerFrequency :: Value -> Either Text TriggerFrequency.TriggerFrequency
jsonToTriggerFrequency =
  Json.decodeNullary
    (Text.pack "TriggerFrequency")
    [ (Text.pack "EveryTime", TriggerFrequency.EveryTime),
      (Text.pack "FirstTimeEachTurn", TriggerFrequency.FirstTimeEachTurn)
    ]
