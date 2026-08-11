module Pawl.Codec.EndingStep where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EndingStep as EndingStep

toJson :: EndingStep.EndingStep -> Value.Value
toJson s = Common.nullary $ case s of
  EndingStep.EndStep -> "EndStep"
  EndingStep.Cleanup -> "Cleanup"

fromJson :: Value.Value -> Either Text.Text EndingStep.EndingStep
fromJson =
  Common.decodeNullary
    "EndingStep"
    [ ("EndStep", EndingStep.EndStep),
      ("Cleanup", EndingStep.Cleanup)
    ]
