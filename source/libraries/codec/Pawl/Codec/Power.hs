-- | The @Power ⇆ Json@ codec (#481).
module Pawl.Codec.Power where

import Data.Text (Text)
import qualified Pawl.Codec.Quantity as Quantity
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Power as Power

powerToJson :: Power.Power -> Value
powerToJson (Power.MkPower q) = Quantity.toJson q

jsonToPower :: Value -> Either Text Power.Power
jsonToPower value = Power.MkPower <$> Quantity.fromJson value
