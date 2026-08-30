{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForbidAttack where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForbidAttack as ForbidAttack

-- | A bare object keyed by the record's field names, Pawl.Codec.ForbidBlock's
-- shape. The tag that picks it is written by Pawl.Codec.Effect's ForbidAttack arm.
codec :: Codec.Codec ForbidAttack.ForbidAttack
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ForbidAttack.duration
  ref <- Fields.required "ref" ObjectRef.codec ForbidAttack.ref
  pure
    ForbidAttack.MkForbidAttack
      { ForbidAttack.duration = duration,
        ForbidAttack.ref = ref
      }
