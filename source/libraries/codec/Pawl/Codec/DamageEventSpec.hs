module Pawl.Codec.DamageEventSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageEvent" $ do
  -- A NONZERO toxic value, so the CR 702.164b rider round-trips rather than
  -- getting defaulted past.
  Spec.it s "MkDamageEvent, dealt to a player" $
    Common.assertJsonCodec
      s
      DamageEvent.toJson
      DamageEvent.fromJson
      DamageEvent.MkDamageEvent
        { DamageEvent.source = ObjectId.MkObjectId 1,
          DamageEvent.target = Recipient.ToPlayer (PlayerId.MkPlayerId 2),
          DamageEvent.amount = 3,
          DamageEvent.dealtByDeathtouch = True,
          DamageEvent.dealtByInfect = False,
          DamageEvent.dealtByToxic = 2,
          DamageEvent.kind = DamageKind.Combat
        }
      "{\"source\":1,\"target\":{\"type\":\"ToPlayer\",\"value\":2},\"amount\":3,\"dealtByDeathtouch\":true,\"dealtByInfect\":false,\"dealtByToxic\":2,\"kind\":{\"type\":\"Combat\"}}"
  -- CR 120.3c's recipient tag is a different arm of Recipient from the one
  -- above, and noncombat damage (CR 608) is a different arm of DamageKind.
  Spec.it s "MkDamageEvent, dealt to a planeswalker" $
    Common.assertJsonCodec
      s
      DamageEvent.toJson
      DamageEvent.fromJson
      DamageEvent.MkDamageEvent
        { DamageEvent.source = ObjectId.MkObjectId 1,
          DamageEvent.target = Recipient.ToPlaneswalker (ObjectId.MkObjectId 5),
          DamageEvent.amount = 4,
          DamageEvent.dealtByDeathtouch = False,
          DamageEvent.dealtByInfect = True,
          DamageEvent.dealtByToxic = 0,
          DamageEvent.kind = DamageKind.Noncombat
        }
      "{\"source\":1,\"target\":{\"type\":\"ToPlaneswalker\",\"value\":5},\"amount\":4,\"dealtByDeathtouch\":false,\"dealtByInfect\":true,\"dealtByToxic\":0,\"kind\":{\"type\":\"Noncombat\"}}"
