module Pawl.Codec.TriggerCondition where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Condition as Condition
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
  TriggerCondition.SelfAttacks f -> Common.tagged "SelfAttacks" . Just $ TriggerFrequency.toJson f
  TriggerCondition.SelfCycled -> Common.nullary "SelfCycled"
  TriggerCondition.PlayerDiscards r -> Common.tagged "PlayerDiscards" . Just $ PlayerRelation.toJson r
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Common.nullary "SelfPutIntoGraveyardFromLibrary"
  TriggerCondition.SelfDies -> Common.nullary "SelfDies"
  TriggerCondition.SelfLeavesTheBattlefield -> Common.nullary "SelfLeavesTheBattlefield"
  TriggerCondition.SpellOrAbilityCounters r -> Common.tagged "SpellOrAbilityCounters" . Just $ PlayerRelation.toJson r

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
    ("SelfAttacks", Just v) -> TriggerCondition.SelfAttacks <$> TriggerFrequency.fromJson v
    ("SelfCycled", _) -> Right TriggerCondition.SelfCycled
    ("PlayerDiscards", Just v) -> TriggerCondition.PlayerDiscards <$> PlayerRelation.fromJson v
    ("SelfPutIntoGraveyardFromLibrary", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromLibrary
    ("SelfDies", _) -> Right TriggerCondition.SelfDies
    ("SelfLeavesTheBattlefield", _) -> Right TriggerCondition.SelfLeavesTheBattlefield
    ("SpellOrAbilityCounters", Just v) -> TriggerCondition.SpellOrAbilityCounters <$> PlayerRelation.fromJson v
    _ -> Left . Text.pack $ "unknown TriggerCondition: " <> t
