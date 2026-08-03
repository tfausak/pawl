module Pawl.Codec.GameEventSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.GameEvent as GameEvent
import qualified Pawl.Codec.ProjectedCharacteristicsSpec as ProjectedCharacteristicsSpec
import qualified Pawl.Spec as Spec
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

-- | R7's ten cases, one per GameEvent constructor. The Moved/Revealed cases
-- carry 'ProjectedCharacteristicsSpec.testCharacteristics' as a stand-in
-- snapshot -- this sublibrary sits above Pawl.Registry and Pawl.Engine, so it
-- cannot build a real one the way Pawl.Engine.Projection.project does. The
-- registry-backed round-trips over REAL snapshots (Typhoid Rats, and the
-- doubled-toxic-keyword count survives the encoding) stay in
-- Pawl.CodecIntegrationSpec.
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
          <> "\"dealtByDeathtouch\":true,\"dealtByInfect\":false,\"dealtByToxic\":3,\"dealtByLifelink\":2,\"kind\":{\"type\":\"Combat\"}}}"
      )
  Spec.it s "StepBegan" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.StepBegan (Phase.Ending EndingStep.EndStep) (PlayerId.MkPlayerId 0))
      "{\"type\":\"StepBegan\",\"value\":[{\"type\":\"Ending\",\"value\":{\"type\":\"EndStep\"}},0]}"
  Spec.it s "SpellCast" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.SpellCast (PlayerId.MkPlayerId 0))
      "{\"type\":\"SpellCast\",\"value\":0}"
  Spec.it s "BecameMonarch" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.BecameMonarch (PlayerId.MkPlayerId 0))
      "{\"type\":\"BecameMonarch\",\"value\":0}"
  -- CR 702.29c/d's two-descriptions-one-event shape: ToPayCyclingCost is the
  -- payload that ALSO satisfies a "cycles or discards" trigger once, not twice.
  Spec.it s "Discarded" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.Discarded (PlayerId.MkPlayerId 0) (ObjectId.MkObjectId 7) DiscardCause.ToPayCyclingCost)
      "{\"type\":\"Discarded\",\"value\":[0,7,{\"type\":\"ToPayCyclingCost\"}]}"
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
      "{\"type\":\"AttackerDeclared\",\"value\":3}"
  Spec.it s "SpellCountered" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.SpellCountered (Countering.MkCountering (ObjectId.MkObjectId 4) (ObjectId.MkObjectId 5) (PlayerId.MkPlayerId 1)))
      "{\"type\":\"SpellCountered\",\"value\":{\"spell\":4,\"source\":5,\"controller\":1}}"
  Spec.it s "LoyaltyAbilityActivated" $
    Common.assertJsonCodec
      s
      GameEvent.toJson
      GameEvent.fromJson
      (GameEvent.LoyaltyAbilityActivated (ObjectId.MkObjectId 7))
      "{\"type\":\"LoyaltyAbilityActivated\",\"value\":7}"
