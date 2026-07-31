-- | The @EndingStep ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.EndingStep where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.EndingStep as EndingStep

endingStepToJson :: EndingStep.EndingStep -> Value
endingStepToJson s = Json.nullary . Text.pack $ case s of
  EndingStep.EndStep -> "EndStep"
  EndingStep.Cleanup -> "Cleanup"

jsonToEndingStep :: Value -> Either Text EndingStep.EndingStep
jsonToEndingStep =
  Json.decodeNullary
    (Text.pack "EndingStep")
    [ (Text.pack "EndStep", EndingStep.EndStep),
      (Text.pack "Cleanup", EndingStep.Cleanup)
    ]
