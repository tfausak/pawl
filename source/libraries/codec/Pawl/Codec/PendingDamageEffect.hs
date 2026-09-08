{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PendingDamageEffect where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.GrantedAbility as GrantedAbility
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PendingDamageEffect as PendingDamageEffect
import qualified Pawl.Types.SlotName as SlotName

-- | Pawl.Codec.PreventionRider's shape, field for field: the snapshotted target
-- map takes 'Common.textMap' for that codec's reason.
codec :: Codec.Codec PendingDamageEffect.PendingDamageEffect
codec = Fields.object $ do
  effects <- Fields.required "effects" (Common.seq (Effect.codec Card.codec (GrantedAbility.codec Card.codec))) PendingDamageEffect.effects
  targets <- Fields.required "targets" (Common.textMap SlotName.unwrap (Right . SlotName.MkSlotName) (Common.set Recipient.codec)) PendingDamageEffect.targets
  controller <- Fields.required "controller" PlayerId.codec PendingDamageEffect.controller
  source <- Fields.required "source" ObjectId.codec PendingDamageEffect.source
  pure
    PendingDamageEffect.MkPendingDamageEffect
      { PendingDamageEffect.effects = effects,
        PendingDamageEffect.targets = targets,
        PendingDamageEffect.controller = controller,
        PendingDamageEffect.source = source
      }
