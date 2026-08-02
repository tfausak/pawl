module Pawl.Codec.GameEvent where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.GameEvent as GameEvent

toJson :: GameEvent.GameEvent -> Value.Value
toJson e = case e of
  GameEvent.Moved zc pc -> Common.tagged "Moved" . Just . Common.array $ [ZoneChange.toJson zc, ProjectedCharacteristics.toJson pc]
  GameEvent.DamageDealt ev -> Common.tagged "DamageDealt" . Just $ DamageEvent.toJson ev
  GameEvent.StepBegan p pid -> Common.tagged "StepBegan" . Just . Common.array $ [Phase.toJson p, PlayerId.toJson pid]
  GameEvent.SpellCast pid -> Common.tagged "SpellCast" . Just $ PlayerId.toJson pid
  GameEvent.BecameMonarch pid -> Common.tagged "BecameMonarch" . Just $ PlayerId.toJson pid
  GameEvent.Discarded pid oid cause ->
    Common.tagged "Discarded" . Just . Common.array $ [PlayerId.toJson pid, ObjectId.toJson oid, DiscardCause.toJson cause]
  GameEvent.Revealed pid pc -> Common.tagged "Revealed" . Just . Common.array $ [PlayerId.toJson pid, ProjectedCharacteristics.toJson pc]
  GameEvent.AttackerDeclared oid -> Common.tagged "AttackerDeclared" . Just $ ObjectId.toJson oid
  GameEvent.SpellCountered c -> Common.tagged "SpellCountered" . Just $ Countering.toJson c
  GameEvent.LoyaltyAbilityActivated oid -> Common.tagged "LoyaltyAbilityActivated" . Just $ ObjectId.toJson oid

fromJson :: Value.Value -> Either Text.Text GameEvent.GameEvent
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Moved", Just (Value.Array (Array.MkArray [zc, pc]))) -> GameEvent.Moved <$> ZoneChange.fromJson zc <*> ProjectedCharacteristics.fromJson pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> DamageEvent.fromJson v
    ("StepBegan", Just (Value.Array (Array.MkArray [p, pid]))) -> GameEvent.StepBegan <$> Phase.fromJson p <*> PlayerId.fromJson pid
    ("SpellCast", Just v) -> GameEvent.SpellCast <$> PlayerId.fromJson v
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> PlayerId.fromJson v
    ("Discarded", Just (Value.Array (Array.MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> PlayerId.fromJson pid <*> ObjectId.fromJson oid <*> DiscardCause.fromJson cause
    ("Revealed", Just (Value.Array (Array.MkArray [pid, pc]))) -> GameEvent.Revealed <$> PlayerId.fromJson pid <*> ProjectedCharacteristics.fromJson pc
    ("AttackerDeclared", Just v) -> GameEvent.AttackerDeclared <$> ObjectId.fromJson v
    ("SpellCountered", Just v) -> GameEvent.SpellCountered <$> Countering.fromJson v
    ("LoyaltyAbilityActivated", Just v) -> GameEvent.LoyaltyAbilityActivated <$> ObjectId.fromJson v
    _ -> Left . Text.pack $ "unknown GameEvent: " <> t
