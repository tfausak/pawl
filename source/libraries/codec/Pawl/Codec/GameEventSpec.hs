{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.GameEventSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.GameEvent as GameEvent
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Spec as Spec
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
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | One case per GameEvent constructor. The Moved/Revealed cases carry a
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
      (GameEvent.DamageDealt (DamageEvent.MkDamageEvent (ObjectId.MkObjectId 1) (Recipient.ToPlayer (PlayerId.MkPlayerId 1)) 2 True False 3 (Just (PlayerId.MkPlayerId 2)) DamageKind.Combat))
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
      (GameEvent.SpellCast (PlayerId.MkPlayerId 0))
      """ {"type":"SpellCast","value":0} """
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
  Spec.it s "AttackerDeclared" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.AttackerDeclared (ObjectId.MkObjectId 3))
      """ {"type":"AttackerDeclared","value":3} """
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
