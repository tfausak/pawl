-- | The @ActivationTiming ⇆ Json@ codec (#481).
module Pawl.Codec.ActivationTiming where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ActivationTiming as ActivationTiming

-- Tagged rather than bare-nullary since CR 500.1's DuringPhase carries a phase,
-- the shape costComponentToJson takes. AnyTime and SorcerySpeed still render as
-- the bare tag they always did, so every committed card file is unchanged.
activationTimingToJson :: ActivationTiming.ActivationTiming -> Value
activationTimingToJson t = case t of
  ActivationTiming.AnyTime -> Json.nullary (Text.pack "AnyTime")
  ActivationTiming.SorcerySpeed -> Json.nullary (Text.pack "SorcerySpeed")
  ActivationTiming.DuringPhase p -> Json.tagged (Text.pack "DuringPhase") (Just (phaseToJson p))

jsonToActivationTiming :: Value -> Either Text ActivationTiming.ActivationTiming
jsonToActivationTiming value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AnyTime", _) -> Right ActivationTiming.AnyTime
    ("SorcerySpeed", _) -> Right ActivationTiming.SorcerySpeed
    ("DuringPhase", Just v) -> ActivationTiming.DuringPhase <$> jsonToPhase v
    _ -> Left (Text.pack "unknown ActivationTiming: " <> t)
