module Pawl.Codec.GameEvent where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.GameEvent as GameEvent

toJson :: GameEvent.GameEvent -> Value.Value
toJson e = case e of
  GameEvent.Moved zc pc -> Common.tagged "Moved" . Just . Common.array $ [ZoneChange.toJson zc, ProjectedCharacteristics.toJson pc]
  GameEvent.DamageDealt ev -> Common.tagged "DamageDealt" . Just $ DamageEvent.toJson ev
  GameEvent.DamagePrevented r n -> Common.tagged "DamagePrevented" . Just . Common.array $ [Recipient.toJson r, Common.encodeNatural n]
  GameEvent.StepBegan p pid -> Common.tagged "StepBegan" . Just . Common.array $ [Phase.toJson p, PlayerId.toJson pid]
  GameEvent.SpellCast pid oid pc -> Common.tagged "SpellCast" . Just . Common.array $ [PlayerId.toJson pid, ObjectId.toJson oid, ProjectedCharacteristics.toJson pc]
  GameEvent.BecameMonarch pid -> Common.tagged "BecameMonarch" . Just $ PlayerId.toJson pid
  GameEvent.Discarded pid oid cause ->
    Common.tagged "Discarded" . Just . Common.array $ [PlayerId.toJson pid, ObjectId.toJson oid, DiscardCause.toJson cause]
  GameEvent.Revealed pid pc -> Common.tagged "Revealed" . Just . Common.array $ [PlayerId.toJson pid, ProjectedCharacteristics.toJson pc]
  GameEvent.AttackerDeclared oid pid -> Common.tagged "AttackerDeclared" . Just . Common.array $ [ObjectId.toJson oid, PlayerId.toJson pid]
  GameEvent.BlockerDeclared blocker attacker -> Common.tagged "BlockerDeclared" . Just . Common.array $ [ObjectId.toJson blocker, ObjectId.toJson attacker]
  GameEvent.AttackerBlocked oid pid -> Common.tagged "AttackerBlocked" . Just . Common.array $ [ObjectId.toJson oid, PlayerId.toJson pid]
  GameEvent.SpellCountered c -> Common.tagged "SpellCountered" . Just $ Countering.toJson c
  GameEvent.LifeLost p n -> Common.tagged "LifeLost" . Just $ Common.array [PlayerId.toJson p, Common.encodeNatural n]
  GameEvent.LifeGained p n -> Common.tagged "LifeGained" . Just $ Common.array [PlayerId.toJson p, Common.encodeNatural n]
  GameEvent.LoyaltyAbilityActivated oid -> Common.tagged "LoyaltyAbilityActivated" . Just $ ObjectId.toJson oid
  GameEvent.CountersPut oid kind before after ->
    Common.tagged "CountersPut" . Just . Common.array $ [ObjectId.toJson oid, CounterKind.toJson kind, Common.encodeNatural before, Common.encodeNatural after]
  GameEvent.CountersRemoved oid kind before after ->
    Common.tagged "CountersRemoved" . Just . Common.array $ [ObjectId.toJson oid, CounterKind.toJson kind, Common.encodeNatural before, Common.encodeNatural after]
  GameEvent.HalfUnlocked oid name fully -> Common.tagged "HalfUnlocked" . Just . Common.array $ [ObjectId.toJson oid, CardName.toJson name, Common.boolean fully]
  GameEvent.TurnedFaceUp oid -> Common.tagged "TurnedFaceUp" . Just $ ObjectId.toJson oid
  GameEvent.PermanentSacrificed pid oid -> Common.tagged "PermanentSacrificed" . Just . Common.array $ [PlayerId.toJson pid, ObjectId.toJson oid]
  GameEvent.AbilityTriggered oid pid cond ->
    Common.tagged "AbilityTriggered" . Just . Common.array $ [ObjectId.toJson oid, PlayerId.toJson pid, TriggerCondition.toJson cond]

fromJson :: Value.Value -> Either Text.Text GameEvent.GameEvent
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Moved", Just (Value.Array (Array.MkArray [zc, pc]))) -> GameEvent.Moved <$> ZoneChange.fromJson zc <*> ProjectedCharacteristics.fromJson pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> DamageEvent.fromJson v
    ("DamagePrevented", Just (Value.Array (Array.MkArray [r, n]))) -> GameEvent.DamagePrevented <$> Recipient.fromJson r <*> Common.decodeNatural n
    ("StepBegan", Just (Value.Array (Array.MkArray [p, pid]))) -> GameEvent.StepBegan <$> Phase.fromJson p <*> PlayerId.fromJson pid
    ("SpellCast", Just (Value.Array (Array.MkArray [pid, oid, pc]))) -> GameEvent.SpellCast <$> PlayerId.fromJson pid <*> ObjectId.fromJson oid <*> ProjectedCharacteristics.fromJson pc
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> PlayerId.fromJson v
    ("Discarded", Just (Value.Array (Array.MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> PlayerId.fromJson pid <*> ObjectId.fromJson oid <*> DiscardCause.fromJson cause
    ("Revealed", Just (Value.Array (Array.MkArray [pid, pc]))) -> GameEvent.Revealed <$> PlayerId.fromJson pid <*> ProjectedCharacteristics.fromJson pc
    ("AttackerDeclared", Just (Value.Array (Array.MkArray [oid, pid]))) -> GameEvent.AttackerDeclared <$> ObjectId.fromJson oid <*> PlayerId.fromJson pid
    ("BlockerDeclared", Just (Value.Array (Array.MkArray [blocker, attacker]))) -> GameEvent.BlockerDeclared <$> ObjectId.fromJson blocker <*> ObjectId.fromJson attacker
    ("AttackerBlocked", Just (Value.Array (Array.MkArray [oid, pid]))) -> GameEvent.AttackerBlocked <$> ObjectId.fromJson oid <*> PlayerId.fromJson pid
    ("SpellCountered", Just v) -> GameEvent.SpellCountered <$> Countering.fromJson v
    ("LifeLost", Just (Value.Array (Array.MkArray [p, n]))) -> GameEvent.LifeLost <$> PlayerId.fromJson p <*> Common.decodeNatural n
    ("LifeGained", Just (Value.Array (Array.MkArray [p, n]))) -> GameEvent.LifeGained <$> PlayerId.fromJson p <*> Common.decodeNatural n
    ("LoyaltyAbilityActivated", Just v) -> GameEvent.LoyaltyAbilityActivated <$> ObjectId.fromJson v
    ("CountersPut", Just (Value.Array (Array.MkArray [oid, kind, before, after]))) ->
      GameEvent.CountersPut <$> ObjectId.fromJson oid <*> CounterKind.fromJson kind <*> Common.decodeNatural before <*> Common.decodeNatural after
    ("CountersRemoved", Just (Value.Array (Array.MkArray [oid, kind, before, after]))) ->
      GameEvent.CountersRemoved <$> ObjectId.fromJson oid <*> CounterKind.fromJson kind <*> Common.decodeNatural before <*> Common.decodeNatural after
    ("HalfUnlocked", Just (Value.Array (Array.MkArray [oid, name, fully]))) -> GameEvent.HalfUnlocked <$> ObjectId.fromJson oid <*> CardName.fromJson name <*> Common.asBoolean fully
    ("TurnedFaceUp", Just v) -> GameEvent.TurnedFaceUp <$> ObjectId.fromJson v
    ("PermanentSacrificed", Just (Value.Array (Array.MkArray [pid, oid]))) -> GameEvent.PermanentSacrificed <$> PlayerId.fromJson pid <*> ObjectId.fromJson oid
    ("AbilityTriggered", Just (Value.Array (Array.MkArray [oid, pid, cond]))) ->
      GameEvent.AbilityTriggered <$> ObjectId.fromJson oid <*> PlayerId.fromJson pid <*> TriggerCondition.fromJson cond
    _ -> Left . Text.pack $ "unknown GameEvent: " <> t
