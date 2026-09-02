{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForbidAttack where

import qualified Pawl.Codec.AimedAt as AimedAt
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForbidAttack as ForbidAttack

-- | A bare object keyed by the record's field names, Pawl.Codec.ForbidBlock's
-- shape with "affected" tagged where that codec's "ref" is bare, and "aimedAt"
-- elided for the restriction on attacking at all. The tag that picks it is
-- written by Pawl.Codec.Effect's ForbidAttack arm.
codec :: Codec.Codec ForbidAttack.ForbidAttack
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ForbidAttack.duration
  affected <- Fields.required "affected" (RestrictedCreatures.codec ObjectRef.codec) ForbidAttack.affected
  aimedAt <- Fields.defaulted "aimedAt" Nothing (Common.maybe AimedAt.codec) ForbidAttack.aimedAt
  pure
    ForbidAttack.MkForbidAttack
      { ForbidAttack.duration = duration,
        ForbidAttack.affected = affected,
        ForbidAttack.aimedAt = aimedAt
      }
