module Pawl.Codec.Duration where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.Duration as Duration

toJson :: Duration.Duration -> Value.Value
toJson d = case d of
  Duration.UntilEndOfTurn -> Common.nullary "UntilEndOfTurn"
  Duration.Indefinite -> Common.nullary "Indefinite"
  Duration.UntilYourNextTurn -> Common.nullary "UntilYourNextTurn"
  Duration.ForAsLongAs c -> Common.tagged "ForAsLongAs" . Just $ Condition.toJson c

fromJson :: Value.Value -> Either Text.Text Duration.Duration
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("UntilEndOfTurn", _) -> Right Duration.UntilEndOfTurn
    ("Indefinite", _) -> Right Duration.Indefinite
    ("UntilYourNextTurn", _) -> Right Duration.UntilYourNextTurn
    ("ForAsLongAs", Just v) -> Duration.ForAsLongAs <$> Condition.fromJson v
    _ -> Left . Text.pack $ "unknown Duration: " <> t
