{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PendingEntryEffect where

import qualified Pawl.Codec.Card as Card
import qualified Pawl.Codec.Effect as Effect
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PendingEntryEffect as PendingEntryEffect

-- | `controller` is read at APPLICATION rather than re-derived at drain time (CR
-- 614.12), so it is stored beside the program and encoded with it.
codec :: Codec.Codec PendingEntryEffect.PendingEntryEffect
codec = Fields.object $ do
  object <- Fields.required "object" ObjectId.codec PendingEntryEffect.object
  controller <- Fields.required "controller" PlayerId.codec PendingEntryEffect.controller
  effects <- Fields.required "effects" (Common.seq (Effect.codec Card.codec)) PendingEntryEffect.effects
  pure
    PendingEntryEffect.MkPendingEntryEffect
      { PendingEntryEffect.object = object,
        PendingEntryEffect.controller = controller,
        PendingEntryEffect.effects = effects
      }
