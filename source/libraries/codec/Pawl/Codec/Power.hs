-- | The @Power ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Power where

import Data.Text (Text)
import Pawl.Codec.Quantity (jsonToQuantity, quantityToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Power as Power

powerToJson :: Power.Power -> Value
powerToJson (Power.MkPower q) = quantityToJson q

jsonToPower :: Value -> Either Text Power.Power
jsonToPower value = Power.MkPower <$> jsonToQuantity value
