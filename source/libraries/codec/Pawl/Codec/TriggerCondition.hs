module Pawl.Codec.TriggerCondition where

import qualified Data.Text as Text
import qualified Pawl.Codec.CardName as CardName
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TriggerCondition as TriggerCondition

toJson :: TriggerCondition.TriggerCondition -> Value.Value
toJson c = case c of
  TriggerCondition.SelfEnters -> Common.nullary "SelfEnters"
  TriggerCondition.PermanentEnters f -> Common.tagged "PermanentEnters" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.StepBegins p s -> Common.tagged "StepBegins" . Just . Common.array $ [Phase.toJson p, TurnScope.toJson s]
  TriggerCondition.StateIs c2 -> Common.tagged "StateIs" . Just $ Condition.toJson c2
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Common.nullary "SelfDealsCombatDamageToPlayer"
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Common.nullary "CreatureDealtCombatDamageToMonarch"
  TriggerCondition.OpponentLostLifeDuringYourTurn -> Common.nullary "OpponentLostLifeDuringYourTurn"
  TriggerCondition.SelfAttacks f -> Common.tagged "SelfAttacks" . Just $ TriggerFrequency.toJson f
  TriggerCondition.SelfCycled -> Common.nullary "SelfCycled"
  TriggerCondition.PlayerDiscards r -> Common.tagged "PlayerDiscards" . Just $ PlayerRelation.toJson r
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Common.nullary "SelfPutIntoGraveyardFromLibrary"
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Common.nullary "SelfPutIntoGraveyardFromAnywhere"
  TriggerCondition.SelfDies -> Common.nullary "SelfDies"
  TriggerCondition.PermanentDies f -> Common.tagged "PermanentDies" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.SelfLeavesTheBattlefield -> Common.nullary "SelfLeavesTheBattlefield"
  TriggerCondition.SpellOrAbilityCounters r -> Common.tagged "SpellOrAbilityCounters" . Just $ PlayerRelation.toJson r
  TriggerCondition.DamageToPlayerPrevented r -> Common.tagged "DamageToPlayerPrevented" . Just $ PlayerRelation.toJson r
  TriggerCondition.PlayerGainsLife r -> Common.tagged "PlayerGainsLife" . Just $ PlayerRelation.toJson r
  TriggerCondition.PlayerLosesLife r -> Common.tagged "PlayerLosesLife" . Just $ PlayerRelation.toJson r
  TriggerCondition.SelfCountersReached kind n -> Common.tagged "SelfCountersReached" . Just . Common.array $ [CounterKind.toJson kind, Common.encodeNatural n]
  TriggerCondition.SelfLastCounterRemoved kind -> Common.tagged "SelfLastCounterRemoved" . Just $ CounterKind.toJson kind
  TriggerCondition.SpellCast f s -> Common.tagged "SpellCast" . Just . Common.array $ [Filter.toJson Keyword.toJson f, TurnScope.toJson s]
  TriggerCondition.SelfHalfUnlocked n -> Common.tagged "SelfHalfUnlocked" . Just $ CardName.toJson n
  TriggerCondition.RoomFullyUnlocked r -> Common.tagged "RoomFullyUnlocked" . Just $ PlayerRelation.toJson r
  -- RECURSIVE, and the only condition that is: an AnyOf holds conditions, so both
  -- directions of this codec call themselves.
  TriggerCondition.AnyOf cs -> Common.tagged "AnyOf" . Just . Common.array $ fmap toJson cs
  TriggerCondition.SelfTurnedFaceUp -> Common.nullary "SelfTurnedFaceUp"
  TriggerCondition.PermanentTurnedFaceUp f -> Common.tagged "PermanentTurnedFaceUp" . Just $ Filter.toJson Keyword.toJson f
  TriggerCondition.PermanentSacrificed -> Common.nullary "PermanentSacrificed"

fromJson :: Value.Value -> Either Text.Text TriggerCondition.TriggerCondition
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("PermanentEnters", Just v) -> TriggerCondition.PermanentEnters <$> Filter.fromJson Keyword.fromJson v
    ("StepBegins", Just (Value.Array (Array.MkArray [p, s]))) -> TriggerCondition.StepBegins <$> Phase.fromJson p <*> TurnScope.fromJson s
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> Condition.fromJson v
    ("SelfDealsCombatDamageToPlayer", _) -> Right TriggerCondition.SelfDealsCombatDamageToPlayer
    ("CreatureDealtCombatDamageToMonarch", _) -> Right TriggerCondition.CreatureDealtCombatDamageToMonarch
    ("OpponentLostLifeDuringYourTurn", _) -> Right TriggerCondition.OpponentLostLifeDuringYourTurn
    ("SelfAttacks", Just v) -> TriggerCondition.SelfAttacks <$> TriggerFrequency.fromJson v
    ("SelfCycled", _) -> Right TriggerCondition.SelfCycled
    ("PlayerDiscards", Just v) -> TriggerCondition.PlayerDiscards <$> PlayerRelation.fromJson v
    ("SelfPutIntoGraveyardFromLibrary", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromLibrary
    ("SelfPutIntoGraveyardFromAnywhere", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromAnywhere
    ("SelfDies", _) -> Right TriggerCondition.SelfDies
    ("PermanentDies", Just v) -> TriggerCondition.PermanentDies <$> Filter.fromJson Keyword.fromJson v
    ("SelfLeavesTheBattlefield", _) -> Right TriggerCondition.SelfLeavesTheBattlefield
    ("SpellOrAbilityCounters", Just v) -> TriggerCondition.SpellOrAbilityCounters <$> PlayerRelation.fromJson v
    ("DamageToPlayerPrevented", Just v) -> TriggerCondition.DamageToPlayerPrevented <$> PlayerRelation.fromJson v
    ("PlayerGainsLife", Just v) -> TriggerCondition.PlayerGainsLife <$> PlayerRelation.fromJson v
    ("PlayerLosesLife", Just v) -> TriggerCondition.PlayerLosesLife <$> PlayerRelation.fromJson v
    ("SelfCountersReached", Just (Value.Array (Array.MkArray [kind, n]))) -> TriggerCondition.SelfCountersReached <$> CounterKind.fromJson kind <*> Common.decodeNatural n
    ("SelfLastCounterRemoved", Just v) -> TriggerCondition.SelfLastCounterRemoved <$> CounterKind.fromJson v
    ("SpellCast", Just (Value.Array (Array.MkArray [f, s]))) -> TriggerCondition.SpellCast <$> Filter.fromJson Keyword.fromJson f <*> TurnScope.fromJson s
    ("SelfHalfUnlocked", Just v) -> TriggerCondition.SelfHalfUnlocked <$> CardName.fromJson v
    ("RoomFullyUnlocked", Just v) -> TriggerCondition.RoomFullyUnlocked <$> PlayerRelation.fromJson v
    ("AnyOf", Just (Value.Array (Array.MkArray cs))) -> TriggerCondition.AnyOf <$> traverse fromJson cs
    ("SelfTurnedFaceUp", _) -> Right TriggerCondition.SelfTurnedFaceUp
    ("PermanentTurnedFaceUp", Just v) -> TriggerCondition.PermanentTurnedFaceUp <$> Filter.fromJson Keyword.fromJson v
    ("PermanentSacrificed", _) -> Right TriggerCondition.PermanentSacrificed
    _ -> Left . Text.pack $ "unknown TriggerCondition: " <> t
