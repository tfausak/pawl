{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForbidBlock where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForbidBlock as ForbidBlock

-- | A bare object keyed by the record's field names, Pawl.Codec.CantBeRegenerated's
-- shape. The tag that picks it is written by Pawl.Codec.Effect's ForbidBlock arm.
codec :: Codec.Codec ForbidBlock.ForbidBlock
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ForbidBlock.duration
  ref <- Fields.required "ref" ObjectRef.codec ForbidBlock.ref
  pure
    ForbidBlock.MkForbidBlock
      { ForbidBlock.duration = duration,
        ForbidBlock.ref = ref
      }
