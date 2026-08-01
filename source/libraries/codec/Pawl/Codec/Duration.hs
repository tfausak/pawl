-- | The @Duration ⇆ Json@ codec (#481).
module Pawl.Codec.Duration where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Duration as Duration

durationToJson :: Duration.Duration -> Value
durationToJson d = case d of
  Duration.UntilEndOfTurn -> Json.nullary (Text.pack "UntilEndOfTurn")
  Duration.Indefinite -> Json.nullary (Text.pack "Indefinite")
  Duration.UntilYourNextTurn -> Json.nullary (Text.pack "UntilYourNextTurn")
  Duration.ForAsLongAs c -> Json.tagged (Text.pack "ForAsLongAs") (Just (Condition.toJson c))

jsonToDuration :: Value -> Either Text Duration.Duration
jsonToDuration value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("UntilEndOfTurn", _) -> Right Duration.UntilEndOfTurn
    ("Indefinite", _) -> Right Duration.Indefinite
    ("UntilYourNextTurn", _) -> Right Duration.UntilYourNextTurn
    ("ForAsLongAs", Just v) -> Duration.ForAsLongAs <$> Condition.fromJson v
    _ -> Left (Text.pack "unknown Duration: " <> t)
