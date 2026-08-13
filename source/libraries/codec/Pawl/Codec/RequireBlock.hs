{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RequireBlock where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RequireBlock as RequireBlock

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's RequireBlock arm.
codec :: Codec.Codec RequireBlock.RequireBlock
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec RequireBlock.duration
  blocker <- Fields.required "blocker" ObjectRef.codec RequireBlock.blocker
  attacker <- Fields.required "attacker" ObjectRef.codec RequireBlock.attacker
  pure
    RequireBlock.MkRequireBlock
      { RequireBlock.duration = duration,
        RequireBlock.blocker = blocker,
        RequireBlock.attacker = attacker
      }
