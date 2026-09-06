{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForbidActivation where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForbidActivation as ForbidActivation

-- | A bare object keyed by the record's field names, Pawl.Codec.ForbidBlock's
-- shape. The tag that picks it is written by Pawl.Codec.Effect's ForbidActivation arm.
codec :: Codec.Codec ForbidActivation.ForbidActivation
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ForbidActivation.duration
  ref <- Fields.required "ref" ObjectRef.codec ForbidActivation.ref
  pure
    ForbidActivation.MkForbidActivation
      { ForbidActivation.duration = duration,
        ForbidActivation.ref = ref
      }
