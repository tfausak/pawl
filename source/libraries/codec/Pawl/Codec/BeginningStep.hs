-- | The @BeginningStep ⇆ Json@ codec (#481).
module Pawl.Codec.BeginningStep where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.BeginningStep as BeginningStep

beginningStepToJson :: BeginningStep.BeginningStep -> Value
beginningStepToJson s = Json.nullary . Text.pack $ case s of
  BeginningStep.Untap -> "Untap"
  BeginningStep.Upkeep -> "Upkeep"
  BeginningStep.DrawStep -> "DrawStep"

jsonToBeginningStep :: Value -> Either Text BeginningStep.BeginningStep
jsonToBeginningStep =
  Json.decodeNullary
    (Text.pack "BeginningStep")
    [ (Text.pack "Untap", BeginningStep.Untap),
      (Text.pack "Upkeep", BeginningStep.Upkeep),
      (Text.pack "DrawStep", BeginningStep.DrawStep)
    ]
