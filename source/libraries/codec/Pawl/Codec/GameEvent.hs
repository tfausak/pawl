-- | The @GameEvent ⇆ Json@ codec (#481).
module Pawl.Codec.GameEvent where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Countering (counteringToJson, jsonToCountering)
import Pawl.Codec.DamageEvent (damageEventToJson, jsonToDamageEvent)
import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ObjectId as ObjectId
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import qualified Pawl.Codec.PlayerId as PlayerId
import Pawl.Codec.ProjectedCharacteristics (jsonToProjectedCharacteristics, projectedCharacteristicsToJson)
import Pawl.Codec.ZoneChange (jsonToZoneChange, zoneChangeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.GameEvent as GameEvent

gameEventToJson :: GameEvent.GameEvent -> Value
gameEventToJson e = case e of
  GameEvent.Moved zc pc -> Json.tagged (Text.pack "Moved") (Just (Array (MkArray [zoneChangeToJson zc, projectedCharacteristicsToJson pc])))
  GameEvent.DamageDealt ev -> Json.tagged (Text.pack "DamageDealt") (Just (damageEventToJson ev))
  GameEvent.StepBegan p pid -> Json.tagged (Text.pack "StepBegan") (Just (Array (MkArray [phaseToJson p, PlayerId.toJson pid])))
  GameEvent.SpellCast pid -> Json.tagged (Text.pack "SpellCast") (Just (PlayerId.toJson pid))
  GameEvent.BecameMonarch pid -> Json.tagged (Text.pack "BecameMonarch") (Just (PlayerId.toJson pid))
  GameEvent.Discarded pid oid cause ->
    Json.tagged (Text.pack "Discarded") (Just (Array (MkArray [PlayerId.toJson pid, ObjectId.toJson oid, DiscardCause.toJson cause])))
  GameEvent.Revealed pid pc -> Json.tagged (Text.pack "Revealed") (Just (Array (MkArray [PlayerId.toJson pid, projectedCharacteristicsToJson pc])))
  GameEvent.AttackerDeclared oid -> Json.tagged (Text.pack "AttackerDeclared") (Just (ObjectId.toJson oid))
  GameEvent.SpellCountered c -> Json.tagged (Text.pack "SpellCountered") (Just (counteringToJson c))
  GameEvent.LoyaltyAbilityActivated oid -> Json.tagged (Text.pack "LoyaltyAbilityActivated") (Just (ObjectId.toJson oid))

jsonToGameEvent :: Value -> Either Text GameEvent.GameEvent
jsonToGameEvent value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("Moved", Just (Array (MkArray [zc, pc]))) -> GameEvent.Moved <$> jsonToZoneChange zc <*> jsonToProjectedCharacteristics pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> jsonToDamageEvent v
    ("StepBegan", Just (Array (MkArray [p, pid]))) -> GameEvent.StepBegan <$> jsonToPhase p <*> PlayerId.fromJson pid
    ("SpellCast", Just v) -> GameEvent.SpellCast <$> PlayerId.fromJson v
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> PlayerId.fromJson v
    ("Discarded", Just (Array (MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> PlayerId.fromJson pid <*> ObjectId.fromJson oid <*> DiscardCause.fromJson cause
    ("Revealed", Just (Array (MkArray [pid, pc]))) -> GameEvent.Revealed <$> PlayerId.fromJson pid <*> jsonToProjectedCharacteristics pc
    ("AttackerDeclared", Just v) -> GameEvent.AttackerDeclared <$> ObjectId.fromJson v
    ("SpellCountered", Just v) -> GameEvent.SpellCountered <$> jsonToCountering v
    ("LoyaltyAbilityActivated", Just v) -> GameEvent.LoyaltyAbilityActivated <$> ObjectId.fromJson v
    _ -> Left (Text.pack "unknown GameEvent: " <> t)

-- MonarchTarget ----------------------------------------------------------------

-- EntryRiders -----------------------------------------------------------------

-- Effect ---------------------------------------------------------------------

-- Records & abilities --------------------------------------------------------
