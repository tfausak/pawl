{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ControlClock where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ControlClock as ControlClock
import qualified Pawl.Types.PlayerId as PlayerId.Type

codec :: Codec.Codec ControlClock.ControlClock
codec = Arm.enum

-- | CR 702.30a's clock for one seat. A pair through 'Pawl.JsonCodec.Common.keyedList',
-- an object key having to be a string. Shared by Pawl.Types.Object.controlClock and
-- Pawl.Types.LastKnown.controlClock, which hold the same map.
entry :: Codec.Codec (PlayerId.Type.PlayerId, ControlClock.ControlClock)
entry = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec fst
  clock <- Fields.required "clock" codec snd
  pure (player, clock)
