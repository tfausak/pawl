-- | The @TriggerCondition ⇆ Json@ codec (#481).
module Pawl.Codec.TriggerCondition where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Phase as Phase
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.Codec.TurnScope as TurnScope
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.TriggerCondition as TriggerCondition

triggerConditionToJson :: TriggerCondition.TriggerCondition -> Value
triggerConditionToJson c = case c of
  TriggerCondition.SelfEnters -> Json.nullary (Text.pack "SelfEnters")
  TriggerCondition.PermanentEnters f -> Json.tagged (Text.pack "PermanentEnters") (Just (Filter.toJson f))
  TriggerCondition.StepBegins p s -> Json.tagged (Text.pack "StepBegins") (Just (Array (MkArray [Phase.toJson p, TurnScope.toJson s])))
  TriggerCondition.StateIs c2 -> Json.tagged (Text.pack "StateIs") (Just (Condition.toJson c2))
  TriggerCondition.SelfDealsCombatDamageToPlayer -> Json.nullary (Text.pack "SelfDealsCombatDamageToPlayer")
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> Json.nullary (Text.pack "CreatureDealtCombatDamageToMonarch")
  TriggerCondition.SelfAttacks f -> Json.tagged (Text.pack "SelfAttacks") (Just (TriggerFrequency.toJson f))
  TriggerCondition.SelfCycled -> Json.nullary (Text.pack "SelfCycled")
  TriggerCondition.PlayerDiscards r -> Json.tagged (Text.pack "PlayerDiscards") (Just (PlayerRelation.toJson r))
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Json.nullary (Text.pack "SelfPutIntoGraveyardFromLibrary")
  TriggerCondition.SelfDies -> Json.nullary (Text.pack "SelfDies")
  TriggerCondition.SelfLeavesTheBattlefield -> Json.nullary (Text.pack "SelfLeavesTheBattlefield")
  TriggerCondition.SpellOrAbilityCounters r -> Json.tagged (Text.pack "SpellOrAbilityCounters") (Just (PlayerRelation.toJson r))

jsonToTriggerCondition :: Value -> Either Text TriggerCondition.TriggerCondition
jsonToTriggerCondition value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("SelfEnters", _) -> Right TriggerCondition.SelfEnters
    ("PermanentEnters", Just v) -> TriggerCondition.PermanentEnters <$> Filter.fromJson v
    ("StepBegins", Just (Array (MkArray [p, s]))) -> TriggerCondition.StepBegins <$> Phase.fromJson p <*> TurnScope.fromJson s
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
    _ -> Left (Text.pack "unknown TriggerCondition: " <> t)

-- Mana, quantity, power/toughness --------------------------------------------
