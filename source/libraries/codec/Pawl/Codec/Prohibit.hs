{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Prohibit where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.Prohibition as Prohibition
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Prohibit as Prohibit

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's Prohibit arm.
codec :: Codec.Codec Prohibit.Prohibit
codec = Fields.object $ do
  what <- Fields.required "what" Prohibition.codec Prohibit.what
  duration <- Fields.required "duration" Duration.codec Prohibit.duration
  ref <- Fields.required "ref" ObjectRef.codec Prohibit.ref
  pure
    Prohibit.MkProhibit
      { Prohibit.what = what,
        Prohibit.duration = duration,
        Prohibit.ref = ref
      }
