{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DamageEventSpec where

import qualified Pawl.Codec.DamageEvent as DamageEvent
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.DamageKind as DamageKind
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DamageEvent" $ do
  -- A NONZERO toxic value, so the CR 702.164b rider round-trips rather than
  -- getting defaulted past, and CR 702.80b's wither bit set so it round-trips as
  -- a present key too. No lifelink, no infect, so dealtByInfect and
  -- dealtByLifelink are both omitted keys.
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
          DamageEvent.dealtByWither = True,
          DamageEvent.dealtByToxic = 2,
          DamageEvent.dealtByLifelink = Nothing,
          DamageEvent.kind = DamageKind.Combat
        }
      """ {"source":1,"target":{"type":"ToPlayer","value":2},"amount":3,"dealtByDeathtouch":true,"dealtByWither":true,"dealtByToxic":2,"kind":{"type":"Combat"}} """
  -- CR 120.3c's recipient tag and CR 608's noncombat damage are each the other
  -- arm of their type. CR 702.15b's lifelink payee is a concrete PlayerId.
  Spec.it s "MkDamageEvent, dealt to a planeswalker, with lifelink" $
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
          DamageEvent.dealtByWither = False,
          DamageEvent.dealtByToxic = 0,
          DamageEvent.dealtByLifelink = Just (PlayerId.MkPlayerId 2),
          DamageEvent.kind = DamageKind.Noncombat
        }
      """ {"source":1,"target":{"type":"ToPlaneswalker","value":5},"amount":4,"dealtByInfect":true,"dealtByLifelink":2,"kind":{"type":"Noncombat"}} """
  -- Every dealtBy* rider at its default, which is what an event carrying none
  -- of them means.
  Spec.it s "an all-default value omits every optional key" $
    Common.assertJsonCodec
      s
      DamageEvent.toJson
      DamageEvent.fromJson
      DamageEvent.MkDamageEvent
        { DamageEvent.source = ObjectId.MkObjectId 1,
          DamageEvent.target = Recipient.ToPlayer (PlayerId.MkPlayerId 2),
          DamageEvent.amount = 3,
          DamageEvent.dealtByDeathtouch = False,
          DamageEvent.dealtByInfect = False,
          DamageEvent.dealtByWither = False,
          DamageEvent.dealtByToxic = 0,
          DamageEvent.dealtByLifelink = Nothing,
          DamageEvent.kind = DamageKind.Combat
        }
      """ {"source":1,"target":{"type":"ToPlayer","value":2},"amount":3,"kind":{"type":"Combat"}} """
