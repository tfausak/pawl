module Pawl.Codec.TurnWindow where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.TurnWindow as TurnWindow

-- | CR 603.7a: the STORED onset, which like Expiry beside it never appears in card
-- JSON -- a card carries an Onset and Pawl.Engine.Event.armOnset turns it into
-- this. The one thing that serialises a TurnWindow is a DelayedTrigger.
toJson :: TurnWindow.TurnWindow -> Value.Value
toJson w = case w of
  TurnWindow.AnyTurn -> Common.nullary "AnyTurn"
  TurnWindow.ControllersNextTurn -> Common.nullary "ControllersNextTurn"
  TurnWindow.OnTurn n -> Common.tagged "OnTurn" . Just $ Common.encodeNatural n

fromJson :: Value.Value -> Either Text.Text TurnWindow.TurnWindow
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AnyTurn", _) -> Right TurnWindow.AnyTurn
    ("ControllersNextTurn", _) -> Right TurnWindow.ControllersNextTurn
    ("OnTurn", Just v) -> TurnWindow.OnTurn <$> Common.decodeNatural v
    _ -> Left . Text.pack $ "unknown TurnWindow: " <> t
