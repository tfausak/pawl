-- | The @Duration ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.Duration where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Condition (conditionToJson, jsonToCondition)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Duration as Duration

durationToJson :: Duration.Duration -> Value
durationToJson d = case d of
  Duration.UntilEndOfTurn -> Json.nullary (Text.pack "UntilEndOfTurn")
  Duration.Indefinite -> Json.nullary (Text.pack "Indefinite")
  Duration.UntilYourNextTurn -> Json.nullary (Text.pack "UntilYourNextTurn")
  Duration.ForAsLongAs c -> Json.tagged (Text.pack "ForAsLongAs") (Just (conditionToJson c))

jsonToDuration :: Value -> Either Text Duration.Duration
jsonToDuration value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("UntilEndOfTurn", _) -> Right Duration.UntilEndOfTurn
    ("Indefinite", _) -> Right Duration.Indefinite
    ("UntilYourNextTurn", _) -> Right Duration.UntilYourNextTurn
    ("ForAsLongAs", Just v) -> Duration.ForAsLongAs <$> jsonToCondition v
    _ -> Left (Text.pack "unknown Duration: " <> t)
