-- | The @GameEvent ⇆ Json@ codec (#481).
module Pawl.Codec.GameEvent where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Countering (counteringToJson, jsonToCountering)
import Pawl.Codec.DamageEvent (damageEventToJson, jsonToDamageEvent)
import Pawl.Codec.DiscardCause (discardCauseToJson, jsonToDiscardCause)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Codec.ProjectedCharacteristics (jsonToProjectedCharacteristics, projectedCharacteristicsToJson)
import Pawl.Codec.ZoneChange (jsonToZoneChange, zoneChangeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.GameEvent as GameEvent

gameEventToJson :: GameEvent.GameEvent -> Value
gameEventToJson e = case e of
  GameEvent.Moved zc pc -> Json.tagged (Text.pack "Moved") (Just (Array (MkArray [zoneChangeToJson zc, projectedCharacteristicsToJson pc])))
  GameEvent.DamageDealt ev -> Json.tagged (Text.pack "DamageDealt") (Just (damageEventToJson ev))
  GameEvent.StepBegan p pid -> Json.tagged (Text.pack "StepBegan") (Just (Array (MkArray [phaseToJson p, playerIdToJson pid])))
  GameEvent.SpellCast pid -> Json.tagged (Text.pack "SpellCast") (Just (playerIdToJson pid))
  GameEvent.BecameMonarch pid -> Json.tagged (Text.pack "BecameMonarch") (Just (playerIdToJson pid))
  GameEvent.Discarded pid oid cause ->
    Json.tagged (Text.pack "Discarded") (Just (Array (MkArray [playerIdToJson pid, objectIdToJson oid, discardCauseToJson cause])))
  GameEvent.Revealed pid pc -> Json.tagged (Text.pack "Revealed") (Just (Array (MkArray [playerIdToJson pid, projectedCharacteristicsToJson pc])))
  GameEvent.AttackerDeclared oid -> Json.tagged (Text.pack "AttackerDeclared") (Just (objectIdToJson oid))
  GameEvent.SpellCountered c -> Json.tagged (Text.pack "SpellCountered") (Just (counteringToJson c))

jsonToGameEvent :: Value -> Either Text GameEvent.GameEvent
jsonToGameEvent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Moved", Just (Array (MkArray [zc, pc]))) -> GameEvent.Moved <$> jsonToZoneChange zc <*> jsonToProjectedCharacteristics pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> jsonToDamageEvent v
    ("StepBegan", Just (Array (MkArray [p, pid]))) -> GameEvent.StepBegan <$> jsonToPhase p <*> jsonToPlayerId pid
    ("SpellCast", Just v) -> GameEvent.SpellCast <$> jsonToPlayerId v
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> jsonToPlayerId v
    ("Discarded", Just (Array (MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> jsonToPlayerId pid <*> jsonToObjectId oid <*> jsonToDiscardCause cause
    ("Revealed", Just (Array (MkArray [pid, pc]))) -> GameEvent.Revealed <$> jsonToPlayerId pid <*> jsonToProjectedCharacteristics pc
    ("AttackerDeclared", Just v) -> GameEvent.AttackerDeclared <$> jsonToObjectId v
    ("SpellCountered", Just v) -> GameEvent.SpellCountered <$> jsonToCountering v
    _ -> Left (Text.pack "unknown GameEvent: " <> t)

-- MonarchTarget ----------------------------------------------------------------

-- TokenEntry -----------------------------------------------------------------

-- Effect ---------------------------------------------------------------------

-- Records & abilities --------------------------------------------------------
