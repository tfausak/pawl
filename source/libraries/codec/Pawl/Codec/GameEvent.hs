module Pawl.Codec.GameEvent where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Codec.Designation as Designation
import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.GameEvent as GameEvent

toJson :: GameEvent.GameEvent -> Value.Value
toJson e = case e of
  GameEvent.Moved zc pc -> Common.tagged "Moved" . Just . Value.array $ [ZoneChange.toJson zc, ProjectedCharacteristics.toJson pc]
  GameEvent.DamageDealt ev -> Common.tagged "DamageDealt" . Just $ DamageEvent.toJson ev
  GameEvent.DamagePrevented r n -> Common.tagged "DamagePrevented" . Just . Value.array $ [Recipient.toJson r, Common.encodeNatural n]
  GameEvent.StepBegan p pid -> Common.tagged "StepBegan" . Just . Value.array $ [Codec.encode Phase.codec p, Codec.encode PlayerId.codec pid]
  GameEvent.SpellCast pid oid pc -> Common.tagged "SpellCast" . Just . Value.array $ [Codec.encode PlayerId.codec pid, ObjectId.toJson oid, ProjectedCharacteristics.toJson pc]
  GameEvent.BecameMonarch pid -> Common.tagged "BecameMonarch" . Just $ Codec.encode PlayerId.codec pid
  GameEvent.Discarded pid oid cause ->
    Common.tagged "Discarded" . Just . Value.array $ [Codec.encode PlayerId.codec pid, ObjectId.toJson oid, DiscardCause.toJson cause]
  GameEvent.Revealed pid pc -> Common.tagged "Revealed" . Just . Value.array $ [Codec.encode PlayerId.codec pid, ProjectedCharacteristics.toJson pc]
  GameEvent.AttackerDeclared oid pid count -> Common.tagged "AttackerDeclared" . Just . Value.array $ [ObjectId.toJson oid, Codec.encode PlayerId.codec pid, Common.encodeNatural count]
  GameEvent.BlockerDeclared blocker attacker -> Common.tagged "BlockerDeclared" . Just . Value.array $ [ObjectId.toJson blocker, ObjectId.toJson attacker]
  GameEvent.BlocksDeclared blocker count -> Common.tagged "BlocksDeclared" . Just . Value.array $ [ObjectId.toJson blocker, Common.encodeNatural count]
  GameEvent.AttackerBlocked oid pid -> Common.tagged "AttackerBlocked" . Just . Value.array $ [ObjectId.toJson oid, Codec.encode PlayerId.codec pid]
  GameEvent.SpellCountered c -> Common.tagged "SpellCountered" . Just $ Countering.toJson c
  GameEvent.LifeLost p n -> Common.tagged "LifeLost" . Just $ Value.array [Codec.encode PlayerId.codec p, Common.encodeNatural n]
  GameEvent.LifeGained p n -> Common.tagged "LifeGained" . Just $ Value.array [Codec.encode PlayerId.codec p, Common.encodeNatural n]
  GameEvent.LoyaltyAbilityActivated oid -> Common.tagged "LoyaltyAbilityActivated" . Just $ ObjectId.toJson oid
  GameEvent.CountersPut oid kind before after ->
    Common.tagged "CountersPut" . Just . Value.array $ [ObjectId.toJson oid, CounterKind.toJson Keyword.toJson kind, Common.encodeNatural before, Common.encodeNatural after]
  GameEvent.CountersRemoved oid kind before after ->
    Common.tagged "CountersRemoved" . Just . Value.array $ [ObjectId.toJson oid, CounterKind.toJson Keyword.toJson kind, Common.encodeNatural before, Common.encodeNatural after]
  GameEvent.HalfUnlocked oid name fully -> Common.tagged "HalfUnlocked" . Just . Value.array $ [ObjectId.toJson oid, CardName.toJson name, Value.boolean fully]
  GameEvent.TurnedFaceUp oid -> Common.tagged "TurnedFaceUp" . Just $ ObjectId.toJson oid
  GameEvent.BecameDesignated d oid -> Common.tagged "BecameDesignated" . Just . Value.array $ [Designation.toJson d, ObjectId.toJson oid]
  GameEvent.Evolved oid -> Common.tagged "Evolved" . Just $ ObjectId.toJson oid
  GameEvent.Mentored mentor mentored -> Common.tagged "Mentored" . Just . Value.array $ [ObjectId.toJson mentor, ObjectId.toJson mentored]
  GameEvent.PermanentSacrificed pid oid -> Common.tagged "PermanentSacrificed" . Just . Value.array $ [Codec.encode PlayerId.codec pid, ObjectId.toJson oid]
  GameEvent.AbilityTriggered oid pid cond ->
    Common.tagged "AbilityTriggered" . Just . Value.array $ [ObjectId.toJson oid, Codec.encode PlayerId.codec pid, TriggerCondition.toJson cond]
  GameEvent.ControlChanged oid before after ->
    Common.tagged "ControlChanged" . Just . Value.array $ [ObjectId.toJson oid, Codec.encode PlayerId.codec before, Codec.encode PlayerId.codec after]

fromJson :: Value.Value -> Either Text.Text GameEvent.GameEvent
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Moved", Just (Value.Array (Array.MkArray [zc, pc]))) -> GameEvent.Moved <$> ZoneChange.fromJson zc <*> ProjectedCharacteristics.fromJson pc
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> DamageEvent.fromJson v
    ("DamagePrevented", Just (Value.Array (Array.MkArray [r, n]))) -> GameEvent.DamagePrevented <$> Recipient.fromJson r <*> Common.decodeNatural n
    ("StepBegan", Just (Value.Array (Array.MkArray [p, pid]))) -> GameEvent.StepBegan <$> Codec.decode Phase.codec p <*> Codec.decode PlayerId.codec pid
    ("SpellCast", Just (Value.Array (Array.MkArray [pid, oid, pc]))) -> GameEvent.SpellCast <$> Codec.decode PlayerId.codec pid <*> ObjectId.fromJson oid <*> ProjectedCharacteristics.fromJson pc
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> Codec.decode PlayerId.codec v
    ("Discarded", Just (Value.Array (Array.MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> Codec.decode PlayerId.codec pid <*> ObjectId.fromJson oid <*> DiscardCause.fromJson cause
    ("Revealed", Just (Value.Array (Array.MkArray [pid, pc]))) -> GameEvent.Revealed <$> Codec.decode PlayerId.codec pid <*> ProjectedCharacteristics.fromJson pc
    ("AttackerDeclared", Just (Value.Array (Array.MkArray [oid, pid, count]))) -> GameEvent.AttackerDeclared <$> ObjectId.fromJson oid <*> Codec.decode PlayerId.codec pid <*> Common.decodeNatural count
    ("BlockerDeclared", Just (Value.Array (Array.MkArray [blocker, attacker]))) -> GameEvent.BlockerDeclared <$> ObjectId.fromJson blocker <*> ObjectId.fromJson attacker
    ("BlocksDeclared", Just (Value.Array (Array.MkArray [blocker, count]))) -> GameEvent.BlocksDeclared <$> ObjectId.fromJson blocker <*> Common.decodeNatural count
    ("AttackerBlocked", Just (Value.Array (Array.MkArray [oid, pid]))) -> GameEvent.AttackerBlocked <$> ObjectId.fromJson oid <*> Codec.decode PlayerId.codec pid
    ("SpellCountered", Just v) -> GameEvent.SpellCountered <$> Countering.fromJson v
    ("LifeLost", Just (Value.Array (Array.MkArray [p, n]))) -> GameEvent.LifeLost <$> Codec.decode PlayerId.codec p <*> Common.decodeNatural n
    ("LifeGained", Just (Value.Array (Array.MkArray [p, n]))) -> GameEvent.LifeGained <$> Codec.decode PlayerId.codec p <*> Common.decodeNatural n
    ("LoyaltyAbilityActivated", Just v) -> GameEvent.LoyaltyAbilityActivated <$> ObjectId.fromJson v
    ("CountersPut", Just (Value.Array (Array.MkArray [oid, kind, before, after]))) ->
      GameEvent.CountersPut <$> ObjectId.fromJson oid <*> CounterKind.fromJson Keyword.fromJson kind <*> Common.decodeNatural before <*> Common.decodeNatural after
    ("CountersRemoved", Just (Value.Array (Array.MkArray [oid, kind, before, after]))) ->
      GameEvent.CountersRemoved <$> ObjectId.fromJson oid <*> CounterKind.fromJson Keyword.fromJson kind <*> Common.decodeNatural before <*> Common.decodeNatural after
    ("HalfUnlocked", Just (Value.Array (Array.MkArray [oid, name, fully]))) -> GameEvent.HalfUnlocked <$> ObjectId.fromJson oid <*> CardName.fromJson name <*> Common.asBoolean fully
    ("TurnedFaceUp", Just v) -> GameEvent.TurnedFaceUp <$> ObjectId.fromJson v
    ("BecameDesignated", Just (Value.Array (Array.MkArray [d, oid]))) ->
      GameEvent.BecameDesignated <$> Designation.fromJson d <*> ObjectId.fromJson oid
    ("Evolved", Just v) -> GameEvent.Evolved <$> ObjectId.fromJson v
    ("Mentored", Just (Value.Array (Array.MkArray [mentor, mentored]))) ->
      GameEvent.Mentored <$> ObjectId.fromJson mentor <*> ObjectId.fromJson mentored
    ("PermanentSacrificed", Just (Value.Array (Array.MkArray [pid, oid]))) -> GameEvent.PermanentSacrificed <$> Codec.decode PlayerId.codec pid <*> ObjectId.fromJson oid
    ("AbilityTriggered", Just (Value.Array (Array.MkArray [oid, pid, cond]))) ->
      GameEvent.AbilityTriggered <$> ObjectId.fromJson oid <*> Codec.decode PlayerId.codec pid <*> TriggerCondition.fromJson cond
    ("ControlChanged", Just (Value.Array (Array.MkArray [oid, before, after]))) ->
      GameEvent.ControlChanged <$> ObjectId.fromJson oid <*> Codec.decode PlayerId.codec before <*> Codec.decode PlayerId.codec after
    _ -> Left . Text.pack $ "unknown GameEvent: " <> t
