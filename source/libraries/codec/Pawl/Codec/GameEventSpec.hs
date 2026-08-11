{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.GameEventSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.GameEvent as GameEvent
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | At least one case per GameEvent constructor. HalfUnlocked gets two, since CR
-- 709.5i's flag is a Bool whose two values are what that rule turns on and a
-- codec that dropped it would round-trip one of them unchanged. The
-- Moved/Revealed/SpellCast cases carry a
-- stand-in snapshot: this sublibrary sits above Pawl.Registry and Pawl.Engine,
-- so it cannot build a real one. The registry-backed round-trips over real
-- snapshots stay in Pawl.CodecIntegrationSpec.
spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GameEvent" $ do
  Spec.it s "Moved" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.Moved (ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) (ObjectId.MkObjectId 2) Zone.Battlefield Zone.Graveyard) ProjectedCharacteristicsSpec.testCharacteristics)
      ( "{\"type\":\"Moved\",\"value\":[{\"departed\":1,\"object\":2,\"from\":{\"type\":\"Battlefield\"},\"to\":{\"type\":\"Graveyard\"}},"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "]}"
      )
  -- A NONZERO toxic value and a PRESENT lifelink payee, so the CR 702.164b and
  -- CR 702.15b riders round-trip rather than getting defaulted past.
  Spec.it s "DamageDealt" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer (PlayerId.MkPlayerId 1)) 2 True False False 3 (Just (PlayerId.MkPlayerId 2)) DamageKind.Combat))
      ( "{\"type\":\"DamageDealt\",\"value\":{\"source\":1,\"target\":{\"type\":\"ToPlayer\",\"value\":1},\"amount\":2,"
          <> "\"dealtByDeathtouch\":true,\"dealtByToxic\":3,\"dealtByLifelink\":2,\"kind\":{\"type\":\"Combat\"}}}"
      )
  -- CR 615.13's record. The recipient is a PLAYER, which is the only shape the
  -- pool's one reader matches.
  Spec.it s "DamagePrevented" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.DamagePrevented (Recipient.ToPlayer (PlayerId.MkPlayerId 1)) 3)
      """ {"type":"DamagePrevented","value":[{"type":"ToPlayer","value":1},3]} """
  Spec.it s "StepBegan" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) (PlayerId.MkPlayerId 0))
      """ {"type":"StepBegan","value":[{"type":"Ending","value":{"type":"EndStep"}},0]} """
  Spec.it s "SpellCast" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.SpellCast (PlayerId.MkPlayerId 0) (ObjectId.MkObjectId 7) ProjectedCharacteristicsSpec.testCharacteristics)
      ( "{\"type\":\"SpellCast\",\"value\":[0,7,"
          <> ProjectedCharacteristicsSpec.testCharacteristicsJson
          <> "]}"
      )
  Spec.it s "BecameMonarch" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.BecameMonarch (PlayerId.MkPlayerId 0))
      """ {"type":"BecameMonarch","value":0} """
  -- CR 702.29c/d's two-descriptions-one-event shape: ToPayCyclingCost is the
  -- payload that ALSO satisfies a "cycles or discards" trigger once, not twice.
  Spec.it s "Discarded" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.Discarded (PlayerId.MkPlayerId 0) (ObjectId.MkObjectId 7) DiscardCause.ToPayCyclingCost)
      """ {"type":"Discarded","value":[0,7,{"type":"ToPayCyclingCost"}]} """
  Spec.it s "Revealed" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.Revealed (PlayerId.MkPlayerId 0) ProjectedCharacteristicsSpec.testCharacteristics)
      ("{\"type\":\"Revealed\",\"value\":[0," <> ProjectedCharacteristicsSpec.testCharacteristicsJson <> "]}")
  -- An object, a player and CR 506.5's declaration size. Three distinct numbers,
  -- so a codec that permuted them would fail.
  Spec.it s "AttackerDeclared" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.AttackerDeclared (ObjectId.MkObjectId 3) (PlayerId.MkPlayerId 1) 4)
      """ {"type":"AttackerDeclared","value":[3,1,4]} """
  -- Two ObjectIds and not an object and a player, unlike the sibling above: CR
  -- 509.1a's declaration pairs a blocker with the creature it blocks. Distinct
  -- numbers, so a codec that swapped the pair would fail.
  Spec.it s "BlockerDeclared" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.BlockerDeclared (ObjectId.MkObjectId 6) (ObjectId.MkObjectId 7))
      """ {"type":"BlockerDeclared","value":[6,7]} """
  -- An object and a COUNT: CR 509.3a's event is per blocking creature, and the
  -- number beside it is how many attackers it took (CR 509.3e). Distinct
  -- numbers again, so a codec that swapped them would fail.
  Spec.it s "BlocksDeclared" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.BlocksDeclared (ObjectId.MkObjectId 9) 2)
      """ {"type":"BlocksDeclared","value":[9,2]} """
  -- An object and a player, AttackerDeclared's shape rather than the sibling
  -- above's two objects: CR 509.3c's event is per blocked ATTACKER, so the
  -- blockers are not in it, and CR 508.5's defending player is.
  Spec.it s "AttackerBlocked" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.AttackerBlocked (ObjectId.MkObjectId 8) (PlayerId.MkPlayerId 2))
      """ {"type":"AttackerBlocked","value":[8,2]} """
  Spec.it s "SpellCountered" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.SpellCountered (Countering.MkCountering (ObjectId.MkObjectId 4) (ObjectId.MkObjectId 5) (PlayerId.MkPlayerId 1)))
      """ {"type":"SpellCountered","value":{"spell":4,"source":5,"controller":1}} """
  Spec.it s "LoyaltyAbilityActivated" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.LoyaltyAbilityActivated (ObjectId.MkObjectId 7))
      """ {"type":"LoyaltyAbilityActivated","value":7} """
  Spec.it s "LifeLost" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.LifeLost (PlayerId.MkPlayerId 1) 2)
      """ {"type":"LifeLost","value":[1,2]} """
  -- CR 119.3's other direction. A DIFFERENT tag from LifeLost above and the same
  -- payload shape, so the two must never decode to each other.
  Spec.it s "LifeGained" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.LifeGained (PlayerId.MkPlayerId 1) 2)
      """ {"type":"LifeGained","value":[1,2]} """
  -- CR 122.6. The two counts are BEFORE then AFTER, in that order, which CR
  -- 714.2b's threshold crossing needs to be able to tell apart.
  Spec.it s "CountersPut" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.CountersPut (ObjectId.MkObjectId 3) CounterKind.Lore 1 2)
      """ {"type":"CountersPut","value":[3,{"type":"Lore"},1,2]} """
  -- CR 310.11b's record, and the pair runs the other way: before > after.
  Spec.it s "CountersRemoved" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.CountersRemoved (ObjectId.MkObjectId 3) CounterKind.Defense 2 0)
      """ {"type":"CountersRemoved","value":[3,{"type":"Defense"},2,0]} """
  -- CR 709.5c's designation, recorded by the permanent and the DOOR: CR 709.5h
  -- tells two unlock triggers on one Room apart by the half's name, so the pair
  -- has to survive the round trip together. The third field is CR 709.5i's
  -- "fully unlocks" flag, and BOTH directions of it are here, since it is exactly
  -- what the two rules disagree about: False is one door of two opening, True is
  -- the last one.
  Spec.it s "HalfUnlocked" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.HalfUnlocked (ObjectId.MkObjectId 4) (CardName.MkCardName (Text.pack "Roaring Furnace")) False)
      """ {"type":"HalfUnlocked","value":[4,"Roaring Furnace",false]} """
  Spec.it s "HalfUnlocked carrying CR 709.5i's fully-unlocked flag" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.HalfUnlocked (ObjectId.MkObjectId 4) (CardName.MkCardName (Text.pack "Steaming Sauna")) True)
      """ {"type":"HalfUnlocked","value":[4,"Steaming Sauna",true]} """
  -- CR 708.7. One id and no more: CR 708.8 makes turning face up a change to one
  -- permanent, and the payload says only which.
  Spec.it s "TurnedFaceUp" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.TurnedFaceUp (ObjectId.MkObjectId 5))
      """ {"type":"TurnedFaceUp","value":5} """
  -- CR 702.112b. One id, TurnedFaceUp's payload exactly: the designation says only
  -- which permanent got it.
  Spec.it s "BecameRenowned" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.BecameRenowned (ObjectId.MkObjectId 5))
      """ {"type":"BecameRenowned","value":5} """
  Spec.it s "Evolved" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.Evolved (ObjectId.MkObjectId 6))
      """ {"type":"Evolved","value":6} """
  -- CR 701.21a: the sacrificing player and the permanent, in that order, and the
  -- id is the PRE-MOVE one -- the record is written before the zone change, which
  -- is CR 603.10a's look-back.
  Spec.it s "PermanentSacrificed" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.PermanentSacrificed (PlayerId.MkPlayerId 0) (ObjectId.MkObjectId 6))
      """ {"type":"PermanentSacrificed","value":[0,6]} """
  -- CR 603.3b: the ability's source (CR 113.7), its controller as it triggered
  -- (CR 603.3a) and the condition that says which ability it was.
  Spec.it s "AbilityTriggered" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.AbilityTriggered (ObjectId.MkObjectId 7) (PlayerId.MkPlayerId 1) (TriggerCondition.SagaFinalChapterTriggers PlayerRelation.You))
      """ {"type":"AbilityTriggered","value":[7,1,{"type":"SagaFinalChapterTriggers","value":{"type":"You"}}]} """
  -- The permanent, then the player control LEFT, then the player it went to. The
  -- order matters and distinct ids prove it: "when YOU lose control" reads the
  -- middle field, and a swap would make it read the gainer.
  Spec.it s "ControlChanged" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.ControlChanged (ObjectId.MkObjectId 8) (PlayerId.MkPlayerId 2) (PlayerId.MkPlayerId 3))
      """ {"type":"ControlChanged","value":[8,2,3]} """
