{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DurationRef where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DurationRef as DurationRef

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by whichever Pawl.Codec.Effect arm carries it, which is what lets
-- three arms share one payload codec without sharing a tag.
codec :: Codec.Codec DurationRef.DurationRef
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec DurationRef.duration
  ref <- Fields.required "ref" ObjectRef.codec DurationRef.ref
  pure
    DurationRef.MkDurationRef
      { DurationRef.duration = duration,
        DurationRef.ref = ref
      }
