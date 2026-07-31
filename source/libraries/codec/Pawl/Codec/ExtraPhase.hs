-- | The @ExtraPhase ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.ExtraPhase where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.ExtraPhase as ExtraPhase

extraPhaseToJson :: ExtraPhase.ExtraPhase -> Value
extraPhaseToJson e = Json.nullary . Text.pack $ case e of
  ExtraPhase.ExtraCombat -> "ExtraCombat"
  ExtraPhase.ExtraMain -> "ExtraMain"

jsonToExtraPhase :: Value -> Either Text ExtraPhase.ExtraPhase
jsonToExtraPhase =
  Json.decodeNullary
    (Text.pack "ExtraPhase")
    [ (Text.pack "ExtraCombat", ExtraPhase.ExtraCombat),
      (Text.pack "ExtraMain", ExtraPhase.ExtraMain)
    ]
