module Pawl.Codec.TriggerConditionSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TriggerCondition as TriggerCondition
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggerCondition" $ do
  -- CR 603.6a: "when this ... enters [the battlefield]".
  Spec.it s "SelfEnters" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfEnters
      "{\"type\":\"SelfEnters\"}"
  -- CR 603.6a's "[type]" is a whole Filter, so the nested And/Not that spells
  -- Soul Warden's "another creature" has to survive the trip.
  Spec.it s "PermanentEnters round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentEnters (Filter.And [Filter.HasCardType CardType.Creature, Filter.Not Filter.IsSource]))
      "{\"type\":\"PermanentEnters\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}}"
  -- CR 603.2b: "at the beginning of [each|your] <step>".
  Spec.it s "StepBegins round-trips" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn)
      "{\"type\":\"StepBegins\",\"value\":[{\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}},{\"type\":\"EachTurn\"}]}"
  -- CR 603.8: a STATE trigger, carrying its Condition.
  Spec.it s "StateIs round-trips" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.StateIs (Condition.MkCondition (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)))
      "{\"type\":\"StateIs\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":0},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":0}}}"
  -- CR 603.2 / 509-510: the bearer dealt combat damage to a player.
  Spec.it s "SelfDealsCombatDamageToPlayer" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfDealsCombatDamageToPlayer
      "{\"type\":\"SelfDealsCombatDamageToPlayer\"}"
  -- CR 725.2: a creature dealt combat damage to the monarch.
  Spec.it s "CreatureDealtCombatDamageToMonarch" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.CreatureDealtCombatDamageToMonarch
      "{\"type\":\"CreatureDealtCombatDamageToMonarch\"}"
  -- CR 702.29c: "'When you cycle this card' means 'When you discard this card to
  -- pay an activation cost of a cycling ability.'"
  Spec.it s "SelfCycled" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfCycled
      "{\"type\":\"SelfCycled\"}"
  -- CR 701.9a's discard, Megrim's "whenever an OPPONENT discards a card". Both
  -- relations, since the PlayerRelation is the whole content of that phrasing.
  Spec.it s "PlayerDiscards round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDiscards PlayerRelation.Opponent)
      "{\"type\":\"PlayerDiscards\",\"value\":{\"type\":\"Opponent\"}}"
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDiscards PlayerRelation.You)
      "{\"type\":\"PlayerDiscards\",\"value\":{\"type\":\"You\"}}"
  -- CR 508.3a: "An ability that reads 'Whenever [a creature] attacks, . . .'
  -- triggers if that creature is declared as an attacker." Both frequencies,
  -- since Aurelia, the Warleader's "for the first time each turn" is a payload
  -- on this condition rather than a sibling one.
  Spec.it s "SelfAttacks round-trips both frequencies" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime)
      "{\"type\":\"SelfAttacks\",\"value\":{\"type\":\"EveryTime\"}}"
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn)
      "{\"type\":\"SelfAttacks\",\"value\":{\"type\":\"FirstTimeEachTurn\"}}"
  -- CR 113.6k's condition (Narcomoeba's), the first that names a zone pair
  -- rather than the battlefield.
  Spec.it s "SelfPutIntoGraveyardFromLibrary" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfPutIntoGraveyardFromLibrary
      "{\"type\":\"SelfPutIntoGraveyardFromLibrary\"}"
  -- CR 603.6c's second written form (Doomed Traveler's), abbreviated by CR 700.4
  -- to "dies".
  Spec.it s "SelfDies" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfDies
      "{\"type\":\"SelfDies\"}"
  -- The same written form read by a bystander (Meren of Clan Nel Toth's
  -- "whenever another creature you control dies"), which carries a Filter where
  -- SelfDies above carries nothing -- so it is a separate tag, and the "another"
  -- lives inside that Filter.
  Spec.it s "PermanentDies round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentDies (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You, Filter.Not Filter.IsSource]))
      "{\"type\":\"PermanentDies\",\"value\":{\"type\":\"And\",\"value\":[{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}},{\"type\":\"Not\",\"value\":{\"type\":\"IsSource\"}}]}}"
  -- CR 603.6c's first written form (Thragtusk's), a separate tag from SelfDies
  -- above: the two must never decode to each other.
  Spec.it s "SelfLeavesTheBattlefield" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfLeavesTheBattlefield
      "{\"type\":\"SelfLeavesTheBattlefield\"}"
  -- CR 701.6a's countering, Baral, Chief of Compliance's "a spell or ability you
  -- control counters a spell". Both relations, for the same reason
  -- PlayerDiscards' does.
  Spec.it s "SpellOrAbilityCounters round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.You)
      "{\"type\":\"SpellOrAbilityCounters\",\"value\":{\"type\":\"You\"}}"
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.Opponent)
      "{\"type\":\"SpellOrAbilityCounters\",\"value\":{\"type\":\"Opponent\"}}"
