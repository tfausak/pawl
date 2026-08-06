{-# LANGUAGE MultilineStrings #-}

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
  -- CR 603.6a.
  Spec.it s "SelfEnters" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfEnters
      """ {"type":"SelfEnters"} """
  -- CR 603.6a's "[type]" is a whole Filter, so a nested And/Not has to survive
  -- the trip.
  Spec.it s "PermanentEnters round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentEnters (Filter.And [Filter.HasCardType CardType.Creature, Filter.Not Filter.IsSource]))
      """ {"type":"PermanentEnters","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"Not","value":{"type":"IsSource"}}]}} """
  -- CR 603.2b.
  Spec.it s "StepBegins round-trips" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.StepBegins (Phase.Ending EndingStep.EndStep) TurnScope.EachTurn)
      """ {"type":"StepBegins","value":[{"type":"Ending","value":{"type":"EndStep"}},{"type":"EachTurn"}]} """
  -- CR 603.8: a STATE trigger, carrying its Condition.
  Spec.it s "StateIs round-trips" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.StateIs (Condition.MkCondition (Quantity.Literal 0) Comparison.Exactly (Quantity.Literal 0)))
      """ {"type":"StateIs","value":{"measured":{"type":"Literal","value":0},"comparison":{"type":"Exactly"},"threshold":{"type":"Literal","value":0}}} """
  -- CR 603.2 / 509-510: the bearer dealt combat damage to a player.
  Spec.it s "SelfDealsCombatDamageToPlayer" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfDealsCombatDamageToPlayer
      """ {"type":"SelfDealsCombatDamageToPlayer"} """
  -- CR 725.2: a creature dealt combat damage to the monarch.
  Spec.it s "CreatureDealtCombatDamageToMonarch" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.CreatureDealtCombatDamageToMonarch
      """ {"type":"CreatureDealtCombatDamageToMonarch"} """
  -- CR 702.179d: one or more opponents lost life during your turn.
  Spec.it s "OpponentLostLifeDuringYourTurn" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.OpponentLostLifeDuringYourTurn
      """ {"type":"OpponentLostLifeDuringYourTurn"} """
  -- CR 702.29c.
  Spec.it s "SelfCycled" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfCycled
      """ {"type":"SelfCycled"} """
  -- CR 701.9a's discard. Both relations, since the PlayerRelation is the whole
  -- content of the "whenever an opponent discards" phrasing.
  Spec.it s "PlayerDiscards round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDiscards PlayerRelation.Opponent)
      """ {"type":"PlayerDiscards","value":{"type":"Opponent"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerDiscards PlayerRelation.You)
      """ {"type":"PlayerDiscards","value":{"type":"You"}} """
  -- CR 508.3a. Both frequencies, since "for the first time each turn" is a
  -- payload on this condition rather than a sibling one.
  Spec.it s "SelfAttacks round-trips both frequencies" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacks TriggerFrequency.EveryTime)
      """ {"type":"SelfAttacks","value":{"type":"EveryTime"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SelfAttacks TriggerFrequency.FirstTimeEachTurn)
      """ {"type":"SelfAttacks","value":{"type":"FirstTimeEachTurn"}} """
  -- CR 113.6k's condition, which names a zone pair rather than the battlefield.
  Spec.it s "SelfPutIntoGraveyardFromLibrary" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfPutIntoGraveyardFromLibrary
      """ {"type":"SelfPutIntoGraveyardFromLibrary"} """
  -- The same rule with no origin zone at all -- a separate tag from the one
  -- above, which it is a superset of: the two must never decode to each other.
  Spec.it s "SelfPutIntoGraveyardFromAnywhere" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfPutIntoGraveyardFromAnywhere
      """ {"type":"SelfPutIntoGraveyardFromAnywhere"} """
  -- CR 603.6c's second written form, abbreviated by CR 700.4 to "dies".
  Spec.it s "SelfDies" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfDies
      """ {"type":"SelfDies"} """
  -- The same written form read by a bystander, which carries a Filter where
  -- SelfDies above carries nothing -- so it is a separate tag, and "another"
  -- lives inside that Filter.
  Spec.it s "PermanentDies round-trips with its Filter" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PermanentDies (Filter.And [Filter.HasCardType CardType.Creature, Filter.ControlledBy PlayerRelation.You, Filter.Not Filter.IsSource]))
      """ {"type":"PermanentDies","value":{"type":"And","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"ControlledBy","value":{"type":"You"}},{"type":"Not","value":{"type":"IsSource"}}]}} """
  -- CR 603.6c's first written form, a separate tag from SelfDies above: the two
  -- must never decode to each other.
  Spec.it s "SelfLeavesTheBattlefield" $
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      TriggerCondition.SelfLeavesTheBattlefield
      """ {"type":"SelfLeavesTheBattlefield"} """
  -- CR 701.6a's countering. Both relations, for the same reason
  -- PlayerDiscards has both.
  Spec.it s "SpellOrAbilityCounters round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.You)
      """ {"type":"SpellOrAbilityCounters","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.SpellOrAbilityCounters PlayerRelation.Opponent)
      """ {"type":"SpellOrAbilityCounters","value":{"type":"Opponent"}} """
  -- CR 615.13's prevention trigger. Both relations, for the same reason.
  Spec.it s "DamageToPlayerPrevented round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.DamageToPlayerPrevented PlayerRelation.You)
      """ {"type":"DamageToPlayerPrevented","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.DamageToPlayerPrevented PlayerRelation.Opponent)
      """ {"type":"DamageToPlayerPrevented","value":{"type":"Opponent"}} """
  -- CR 119.9's life-gain trigger. Both relations, for the same reason.
  Spec.it s "PlayerGainsLife round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerGainsLife PlayerRelation.You)
      """ {"type":"PlayerGainsLife","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerGainsLife PlayerRelation.Opponent)
      """ {"type":"PlayerGainsLife","value":{"type":"Opponent"}} """
  -- The life-LOSS trigger. A DIFFERENT tag from PlayerGainsLife above and the
  -- same payload shape, so the two must never decode to each other -- the same
  -- hazard GameEvent's LifeLost/LifeGained pair carries.
  Spec.it s "PlayerLosesLife round-trips both relations" $ do
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerLosesLife PlayerRelation.You)
      """ {"type":"PlayerLosesLife","value":{"type":"You"}} """
    Common.assertJsonCodec
      s
      TriggerCondition.toJson
      TriggerCondition.fromJson
      (TriggerCondition.PlayerLosesLife PlayerRelation.Opponent)
      """ {"type":"PlayerLosesLife","value":{"type":"Opponent"}} """
