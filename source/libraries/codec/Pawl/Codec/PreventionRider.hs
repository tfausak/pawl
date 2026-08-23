{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PreventionRider where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Recipient as Recipient
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PreventionRider as PreventionRider
import qualified Pawl.Types.SlotName as SlotName

-- | The snapshotted target map is keyed by a slot name, which is a Text newtype,
-- so it takes 'Common.textMap' rather than 'Common.naturalMap' -- the same wrap
-- Pawl.Codec.Binding and Pawl.Codec.TargetSlot pass.
codec :: Codec.Codec PreventionRider.PreventionRider
codec = Fields.object $ do
  effects <- Fields.required "effects" (Common.seq (Effect.codec Card.codec)) PreventionRider.effects
  targets <- Fields.required "targets" (Common.textMap SlotName.unwrap (Right . SlotName.MkSlotName) (Common.set Recipient.codec)) PreventionRider.targets
  controller <- Fields.required "controller" PlayerId.codec PreventionRider.controller
  source <- Fields.required "source" ObjectId.codec PreventionRider.source
  pure
    PreventionRider.MkPreventionRider
      { PreventionRider.effects = effects,
        PreventionRider.targets = targets,
        PreventionRider.controller = controller,
        PreventionRider.source = source
      }
