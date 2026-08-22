{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CantBeRegenerated where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated

-- | A bare object keyed by the record's field names, Pawl.Codec.RequireBlock's
-- shape. The tag that picks it is written by Pawl.Codec.Effect's
-- CantBeRegenerated arm.
codec :: Codec.Codec CantBeRegenerated.CantBeRegenerated
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec CantBeRegenerated.duration
  ref <- Fields.required "ref" ObjectRef.codec CantBeRegenerated.ref
  pure
    CantBeRegenerated.MkCantBeRegenerated
      { CantBeRegenerated.duration = duration,
        CantBeRegenerated.ref = ref
      }
