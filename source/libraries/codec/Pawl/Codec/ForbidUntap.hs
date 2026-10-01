{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForbidUntap where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForbidUntap as ForbidUntap

-- | A bare object keyed by the record's field names, Pawl.Codec.ForbidActivation's
-- shape. The tag that picks it is written by Pawl.Codec.Effect's ForbidUntap arm.
codec :: Codec.Codec ForbidUntap.ForbidUntap
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ForbidUntap.duration
  ref <- Fields.required "ref" ObjectRef.codec ForbidUntap.ref
  pure
    ForbidUntap.MkForbidUntap
      { ForbidUntap.duration = duration,
        ForbidUntap.ref = ref
      }
