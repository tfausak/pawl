-- | The @TriggerCondition ⇆ Json@ codec (#481).
module Pawl.Codec.TriggerCondition where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.Condition (conditionToJson, jsonToCondition)
import Pawl.Codec.Filter (filterToJson, jsonToFilter)
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Phase (jsonToPhase, phaseToJson)
import Pawl.Codec.PlayerRelation (jsonToPlayerRelation, playerRelationToJson)
import Pawl.Codec.TriggerFrequency (jsonToTriggerFrequency, triggerFrequencyToJson)
import Pawl.Codec.TurnScope (jsonToTurnScope, turnScopeToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.TriggerCondition as TriggerCondition

triggerConditionToJson :: TriggerCondition.TriggerCondition -> Value
triggerConditionToJson c = case c of
  TriggerCondition.SelfEnters -> Json.nullary (Text.pack "SelfEnters")
  TriggerCondition.PermanentEnters f -> Json.tagged (Text.pack "PermanentEnters") (Just (filterToJson f))
  TriggerCondition.StepBegins p s -> Json.tagged (Text.pack "StepBegins") (Just (Array (MkArray [phaseToJson p, turnScopeToJson s])))
  TriggerCondition.StateIs c2 -> Json.tagged (Text.pack "StateIs") (Just (conditionToJson c2))
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Json.nullary (Text.pack "SelfDealsCombatDamageToPlayer")
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Json.nullary (Text.pack "CreatureDealtCombatDamageToMonarch")
  TriggerCondition.SelfAttacks f -> Json.tagged (Text.pack "SelfAttacks") (Just (triggerFrequencyToJson f))
  TriggerCondition.SelfCycled -> Json.nullary (Text.pack "SelfCycled")
  TriggerCondition.PlayerDiscards r -> Json.tagged (Text.pack "PlayerDiscards") (Just (playerRelationToJson r))
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Json.nullary (Text.pack "SelfPutIntoGraveyardFromLibrary")
  TriggerCondition.SelfDies -> Json.nullary (Text.pack "SelfDies")
  TriggerCondition.SelfLeavesTheBattlefield -> Json.nullary (Text.pack "SelfLeavesTheBattlefield")
  TriggerCondition.SpellOrAbilityCounters r -> Json.tagged (Text.pack "SpellOrAbilityCounters") (Just (playerRelationToJson r))

jsonToTriggerCondition :: Value -> Either Text TriggerCondition.TriggerCondition
jsonToTriggerCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("PermanentEnters", Just v) -> TriggerCondition.PermanentEnters <$> jsonToFilter v
    ("StepBegins", Just (Array (MkArray [p, s]))) -> TriggerCondition.StepBegins <$> jsonToPhase p <*> jsonToTurnScope s
    ("StateIs", Just v) -> TriggerCondition.StateIs <$> jsonToCondition v
    ("SelfDealsCombatDamageToPlayer", _) -> Right TriggerCondition.SelfDealsCombatDamageToPlayer
    ("CreatureDealtCombatDamageToMonarch", _) -> Right TriggerCondition.CreatureDealtCombatDamageToMonarch
    ("SelfAttacks", Just v) -> TriggerCondition.SelfAttacks <$> jsonToTriggerFrequency v
    ("SelfCycled", _) -> Right TriggerCondition.SelfCycled
    ("PlayerDiscards", Just v) -> TriggerCondition.PlayerDiscards <$> jsonToPlayerRelation v
    ("SelfPutIntoGraveyardFromLibrary", _) -> Right TriggerCondition.SelfPutIntoGraveyardFromLibrary
    ("SelfDies", _) -> Right TriggerCondition.SelfDies
    ("SelfLeavesTheBattlefield", _) -> Right TriggerCondition.SelfLeavesTheBattlefield
    ("SpellOrAbilityCounters", Just v) -> TriggerCondition.SpellOrAbilityCounters <$> jsonToPlayerRelation v
    _ -> Left (Text.pack "unknown TriggerCondition: " <> t)

-- Mana, quantity, power/toughness --------------------------------------------
