{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActiveReplacement where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.PreventionRider as PreventionRider
import qualified Pawl.Codec.ReplacementEffect as ReplacementEffect
import qualified Pawl.Codec.ReplacementOrigin as ReplacementOrigin
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.SlotName as SlotName

-- | The effect codec is CLOSED here rather than a parameter, unlike
-- Pawl.Codec.ReplacementEffect's: the type fixes it at
-- @ReplacementEffect (Effect Card)@, so there is nothing to thread.
--
-- `slots` is keyed by a slot name, which is a Text newtype, so it takes
-- 'Common.textMap' -- the wrap Pawl.Codec.Binding and Pawl.Codec.PreventionRider
-- pass. Its VALUE is a set of ids rather than one, because CR 603.7c's snapshot
-- can name several.
codec :: Codec.Codec ActiveReplacement.ActiveReplacement
codec = Fields.object $ do
  effect <- Fields.required "effect" (ReplacementEffect.codec (Effect.codec Card.codec)) ActiveReplacement.effect
  source <- Fields.required "source" ObjectId.codec ActiveReplacement.source
  controller <- Fields.required "controller" PlayerId.codec ActiveReplacement.controller
  timestamp <- Fields.required "timestamp" Timestamp.codec ActiveReplacement.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActiveReplacement.expiry
  uses <- Fields.required "uses" Uses.codec ActiveReplacement.uses
  origin <- Fields.required "origin" ReplacementOrigin.codec ActiveReplacement.origin
  condition <- Fields.required "condition" (Common.maybe Condition.codec) ActiveReplacement.condition
  rider <- Fields.required "rider" (Common.maybe PreventionRider.codec) ActiveReplacement.rider
  slots <-
    Fields.required
      "slots"
      (Common.textMap SlotName.unwrap (Right . SlotName.MkSlotName) (Common.set ObjectId.codec))
      ActiveReplacement.slots
  pure
    ActiveReplacement.MkActiveReplacement
      { ActiveReplacement.effect = effect,
        ActiveReplacement.source = source,
        ActiveReplacement.controller = controller,
        ActiveReplacement.timestamp = timestamp,
        ActiveReplacement.expiry = expiry,
        ActiveReplacement.uses = uses,
        ActiveReplacement.origin = origin,
        ActiveReplacement.condition = condition,
        ActiveReplacement.rider = rider,
        ActiveReplacement.slots = slots
      }
