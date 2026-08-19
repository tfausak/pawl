{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DamageEvent where

import qualified Pawl.Codec.DamageKind as DamageKind
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DamageEvent as DamageEvent

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
codec :: Codec.Codec DamageEvent.DamageEvent
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec DamageEvent.source
  target <- Fields.required "target" Recipient.codec DamageEvent.target
  amount <- Fields.required "amount" Common.natural DamageEvent.amount
  dealtByDeathtouch <- Fields.defaulted "dealtByDeathtouch" False Common.boolean DamageEvent.dealtByDeathtouch
  dealtByInfect <- Fields.defaulted "dealtByInfect" False Common.boolean DamageEvent.dealtByInfect
  dealtByWither <- Fields.defaulted "dealtByWither" False Common.boolean DamageEvent.dealtByWither
  dealtByToxic <- Fields.defaulted "dealtByToxic" 0 Common.natural DamageEvent.dealtByToxic
  -- CR 702.15b's answer is a player or nobody, so Nothing (no lifelink at deal
  -- time) is what an absent key means.
  dealtByLifelink <- Fields.defaulted "dealtByLifelink" Nothing (Common.maybe PlayerId.codec) DamageEvent.dealtByLifelink
  kind <- Fields.required "kind" DamageKind.codec DamageEvent.kind
  pure
    DamageEvent.MkDamageEvent
      { DamageEvent.source = source,
        DamageEvent.target = target,
        DamageEvent.amount = amount,
        DamageEvent.dealtByDeathtouch = dealtByDeathtouch,
        DamageEvent.dealtByInfect = dealtByInfect,
        DamageEvent.dealtByWither = dealtByWither,
        DamageEvent.dealtByToxic = dealtByToxic,
        DamageEvent.dealtByLifelink = dealtByLifelink,
        DamageEvent.kind = kind
      }
