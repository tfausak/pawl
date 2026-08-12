module Pawl.Codec.ExtraPhase where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ExtraPhase as ExtraPhase

toJson :: ExtraPhase.ExtraPhase -> Value.Value
toJson e = Common.nullary $ case e of
  ExtraPhase.ExtraCombat -> "ExtraCombat"
  ExtraPhase.ExtraMain -> "ExtraMain"

fromJson :: Value.Value -> Either Text.Text ExtraPhase.ExtraPhase
fromJson =
  Common.decodeNullary
    "ExtraPhase"
    [ ("ExtraCombat", ExtraPhase.ExtraCombat),
      ("ExtraMain", ExtraPhase.ExtraMain)
    ]
