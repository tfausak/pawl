module Pawl.Codec.BeginningStep where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.BeginningStep as BeginningStep

toJson :: BeginningStep.BeginningStep -> Value.Value
toJson s = Common.nullary $ case s of
  BeginningStep.Untap -> "Untap"
  BeginningStep.Upkeep -> "Upkeep"
  BeginningStep.DrawStep -> "DrawStep"

fromJson :: Value.Value -> Either Text.Text BeginningStep.BeginningStep
fromJson =
  Common.decodeNullary
    "BeginningStep"
    [ ("Untap", BeginningStep.Untap),
      ("Upkeep", BeginningStep.Upkeep),
      ("DrawStep", BeginningStep.DrawStep)
    ]
