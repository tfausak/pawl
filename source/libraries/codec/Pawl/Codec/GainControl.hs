{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.GainControl where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GainControl as GainControl
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names. `to` elided is "you", the
-- effect's controller (CR 109.5).
codec :: Codec.Codec GainControl.GainControl
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec GainControl.duration
  ref <- Fields.required "ref" ObjectRef.codec GainControl.ref
  to <- Fields.defaulted "to" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec GainControl.to
  pure
    GainControl.MkGainControl
      { GainControl.duration = duration,
        GainControl.ref = ref,
        GainControl.to = to
      }
