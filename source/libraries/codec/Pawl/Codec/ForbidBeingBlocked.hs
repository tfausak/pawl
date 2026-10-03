{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ForbidBeingBlocked where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ForbidBeingBlocked as ForbidBeingBlocked

-- | A bare object keyed by the record's field names, Pawl.Codec.ForbidUntap's
-- shape. The tag that picks it is written by Pawl.Codec.Effect's
-- ForbidBeingBlocked arm.
codec :: Codec.Codec ForbidBeingBlocked.ForbidBeingBlocked
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec ForbidBeingBlocked.duration
  affected <- Fields.required "affected" (Filter.codec Keyword.codec) ForbidBeingBlocked.affected
  pure
    ForbidBeingBlocked.MkForbidBeingBlocked
      { ForbidBeingBlocked.duration = duration,
        ForbidBeingBlocked.affected = affected
      }
