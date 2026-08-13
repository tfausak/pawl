module Pawl.Codec.GameEvent where

import qualified Data.Text as Text
import qualified Pawl.Codec.AttackerBlocked as AttackerBlocked
import qualified Pawl.Codec.BecameDesignated as BecameDesignated
import qualified Pawl.Codec.BlockerDeclared as BlockerDeclared
import qualified Pawl.Codec.BlocksDeclared as BlocksDeclared
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Codec.DamagePrevented as DamagePrevented
import qualified Pawl.Codec.DiscardCause as DiscardCause
import qualified Pawl.Codec.Drew as Drew
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.LifeChange as LifeChange
import qualified Pawl.Codec.Mentored as Mentored
import qualified Pawl.Codec.Moved as Moved
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PermanentSacrificed as PermanentSacrificed
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Codec.RevealCause as RevealCause
import qualified Pawl.Codec.RoomIndex as RoomIndex
import qualified Pawl.Codec.StepBegan as StepBegan
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.GameEvent as GameEvent

-- | Still a loose toJson/fromJson pair rather than a bundle: every multi-payload
-- arm here writes a positional array and owes a record first (#1458).
toJson :: GameEvent.GameEvent -> Value.Value
toJson e = case e of
  GameEvent.Moved x -> Common.tagged "Moved" . Just $ Codec.encode Moved.codec x
  GameEvent.DamageDealt ev -> Common.tagged "DamageDealt" . Just $ Codec.encode DamageEvent.codec ev
  GameEvent.DamagePrevented x -> Common.tagged "DamagePrevented" . Just $ Codec.encode DamagePrevented.codec x
  GameEvent.StepBegan x -> Common.tagged "StepBegan" . Just $ Codec.encode StepBegan.codec x
  GameEvent.SpellCast pid oid pc -> Common.tagged "SpellCast" . Just . Value.array $ [Codec.encode PlayerId.codec pid, Codec.encode ObjectId.codec oid, Codec.encode ProjectedCharacteristics.codec pc]
  GameEvent.BecameMonarch pid -> Common.tagged "BecameMonarch" . Just $ Codec.encode PlayerId.codec pid
  GameEvent.Discarded pid oid cause ->
    Common.tagged "Discarded" . Just . Value.array $ [Codec.encode PlayerId.codec pid, Codec.encode ObjectId.codec oid, Codec.encode DiscardCause.codec cause]
  GameEvent.Drew x -> Common.tagged "Drew" . Just $ Codec.encode Drew.codec x
  GameEvent.Revealed pid oid cause pc ->
    Common.tagged "Revealed" . Just . Value.array $ [Codec.encode PlayerId.codec pid, Codec.encode ObjectId.codec oid, Codec.encode RevealCause.codec cause, Codec.encode ProjectedCharacteristics.codec pc]
  GameEvent.AttackerDeclared oid pid count -> Common.tagged "AttackerDeclared" . Just . Value.array $ [Codec.encode ObjectId.codec oid, Codec.encode PlayerId.codec pid, Common.encodeNatural count]
  GameEvent.BlockerDeclared x -> Common.tagged "BlockerDeclared" . Just $ Codec.encode BlockerDeclared.codec x
  GameEvent.BlocksDeclared x -> Common.tagged "BlocksDeclared" . Just $ Codec.encode BlocksDeclared.codec x
  GameEvent.AttackerBlocked x -> Common.tagged "AttackerBlocked" . Just $ Codec.encode AttackerBlocked.codec x
  GameEvent.SpellCountered c -> Common.tagged "SpellCountered" . Just $ Codec.encode Countering.codec c
  GameEvent.LifeLost x -> Common.tagged "LifeLost" . Just $ Codec.encode LifeChange.codec x
  GameEvent.LifeGained x -> Common.tagged "LifeGained" . Just $ Codec.encode LifeChange.codec x
  GameEvent.LoyaltyAbilityActivated oid -> Common.tagged "LoyaltyAbilityActivated" . Just $ Codec.encode ObjectId.codec oid
  GameEvent.CountersPut oid kind before after ->
    Common.tagged "CountersPut" . Just . Value.array $ [Codec.encode ObjectId.codec oid, Codec.encode (CounterKind.codec Keyword.codec) kind, Common.encodeNatural before, Common.encodeNatural after]
  GameEvent.CountersRemoved oid kind before after ->
    Common.tagged "CountersRemoved" . Just . Value.array $ [Codec.encode ObjectId.codec oid, Codec.encode (CounterKind.codec Keyword.codec) kind, Common.encodeNatural before, Common.encodeNatural after]
  GameEvent.HalfUnlocked oid name fully -> Common.tagged "HalfUnlocked" . Just . Value.array $ [Codec.encode ObjectId.codec oid, Codec.encode CardName.codec name, Value.boolean fully]
  GameEvent.TurnedFaceUp oid -> Common.tagged "TurnedFaceUp" . Just $ Codec.encode ObjectId.codec oid
  GameEvent.BecameDesignated x -> Common.tagged "BecameDesignated" . Just $ Codec.encode BecameDesignated.codec x
  GameEvent.Evolved oid -> Common.tagged "Evolved" . Just $ Codec.encode ObjectId.codec oid
  GameEvent.Mentored x -> Common.tagged "Mentored" . Just $ Codec.encode Mentored.codec x
  GameEvent.PermanentSacrificed x -> Common.tagged "PermanentSacrificed" . Just $ Codec.encode PermanentSacrificed.codec x
  GameEvent.AbilityTriggered oid pid cond ->
    Common.tagged "AbilityTriggered" . Just . Value.array $ [Codec.encode ObjectId.codec oid, Codec.encode PlayerId.codec pid, Codec.encode TriggerCondition.codec cond]
  GameEvent.ControlChanged oid before after ->
    Common.tagged "ControlChanged" . Just . Value.array $ [Codec.encode ObjectId.codec oid, Codec.encode PlayerId.codec before, Codec.encode PlayerId.codec after]
  GameEvent.VentureMarkerEntered pid oid room ->
    Common.tagged "VentureMarkerEntered" . Just . Value.array $ [Codec.encode PlayerId.codec pid, Codec.encode ObjectId.codec oid, Codec.encode RoomIndex.codec room]

fromJson :: Value.Value -> Either Text.Text GameEvent.GameEvent
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("Moved", Just v) -> GameEvent.Moved <$> Codec.decode Moved.codec v
    ("DamageDealt", Just v) -> GameEvent.DamageDealt <$> Codec.decode DamageEvent.codec v
    ("DamagePrevented", Just v) -> GameEvent.DamagePrevented <$> Codec.decode DamagePrevented.codec v
    ("StepBegan", Just v) -> GameEvent.StepBegan <$> Codec.decode StepBegan.codec v
    ("SpellCast", Just (Value.Array (Array.MkArray [pid, oid, pc]))) -> GameEvent.SpellCast <$> Codec.decode PlayerId.codec pid <*> Codec.decode ObjectId.codec oid <*> Codec.decode ProjectedCharacteristics.codec pc
    ("BecameMonarch", Just v) -> GameEvent.BecameMonarch <$> Codec.decode PlayerId.codec v
    ("Discarded", Just (Value.Array (Array.MkArray [pid, oid, cause]))) ->
      GameEvent.Discarded <$> Codec.decode PlayerId.codec pid <*> Codec.decode ObjectId.codec oid <*> Codec.decode DiscardCause.codec cause
    ("Drew", Just v) -> GameEvent.Drew <$> Codec.decode Drew.codec v
    ("Revealed", Just (Value.Array (Array.MkArray [pid, oid, cause, pc]))) ->
      GameEvent.Revealed <$> Codec.decode PlayerId.codec pid <*> Codec.decode ObjectId.codec oid <*> Codec.decode RevealCause.codec cause <*> Codec.decode ProjectedCharacteristics.codec pc
    ("AttackerDeclared", Just (Value.Array (Array.MkArray [oid, pid, count]))) -> GameEvent.AttackerDeclared <$> Codec.decode ObjectId.codec oid <*> Codec.decode PlayerId.codec pid <*> Common.decodeNatural count
    ("BlockerDeclared", Just v) -> GameEvent.BlockerDeclared <$> Codec.decode BlockerDeclared.codec v
    ("BlocksDeclared", Just v) -> GameEvent.BlocksDeclared <$> Codec.decode BlocksDeclared.codec v
    ("AttackerBlocked", Just v) -> GameEvent.AttackerBlocked <$> Codec.decode AttackerBlocked.codec v
    ("SpellCountered", Just v) -> GameEvent.SpellCountered <$> Codec.decode Countering.codec v
    ("LifeLost", Just v) -> GameEvent.LifeLost <$> Codec.decode LifeChange.codec v
    ("LifeGained", Just v) -> GameEvent.LifeGained <$> Codec.decode LifeChange.codec v
    ("LoyaltyAbilityActivated", Just v) -> GameEvent.LoyaltyAbilityActivated <$> Codec.decode ObjectId.codec v
    ("CountersPut", Just (Value.Array (Array.MkArray [oid, kind, before, after]))) ->
      GameEvent.CountersPut <$> Codec.decode ObjectId.codec oid <*> Codec.decode (CounterKind.codec Keyword.codec) kind <*> Common.decodeNatural before <*> Common.decodeNatural after
    ("CountersRemoved", Just (Value.Array (Array.MkArray [oid, kind, before, after]))) ->
      GameEvent.CountersRemoved <$> Codec.decode ObjectId.codec oid <*> Codec.decode (CounterKind.codec Keyword.codec) kind <*> Common.decodeNatural before <*> Common.decodeNatural after
    ("HalfUnlocked", Just (Value.Array (Array.MkArray [oid, name, fully]))) -> GameEvent.HalfUnlocked <$> Codec.decode ObjectId.codec oid <*> Codec.decode CardName.codec name <*> Common.asBoolean fully
    ("TurnedFaceUp", Just v) -> GameEvent.TurnedFaceUp <$> Codec.decode ObjectId.codec v
    ("BecameDesignated", Just v) -> GameEvent.BecameDesignated <$> Codec.decode BecameDesignated.codec v
    ("Evolved", Just v) -> GameEvent.Evolved <$> Codec.decode ObjectId.codec v
    ("Mentored", Just v) -> GameEvent.Mentored <$> Codec.decode Mentored.codec v
    ("PermanentSacrificed", Just v) -> GameEvent.PermanentSacrificed <$> Codec.decode PermanentSacrificed.codec v
    ("AbilityTriggered", Just (Value.Array (Array.MkArray [oid, pid, cond]))) ->
      GameEvent.AbilityTriggered <$> Codec.decode ObjectId.codec oid <*> Codec.decode PlayerId.codec pid <*> Codec.decode TriggerCondition.codec cond
    ("ControlChanged", Just (Value.Array (Array.MkArray [oid, before, after]))) ->
      GameEvent.ControlChanged <$> Codec.decode ObjectId.codec oid <*> Codec.decode PlayerId.codec before <*> Codec.decode PlayerId.codec after
    ("VentureMarkerEntered", Just (Value.Array (Array.MkArray [pid, oid, room]))) ->
      GameEvent.VentureMarkerEntered <$> Codec.decode PlayerId.codec pid <*> Codec.decode ObjectId.codec oid <*> Codec.decode RoomIndex.codec room
    _ -> Left . Text.pack $ "unknown GameEvent: " <> t
