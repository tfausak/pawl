{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CopyStackObject where

import qualified Pawl.Codec.CopyTargets as CopyTargets
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CopyStackObject as CopyStackObject
import qualified Pawl.Types.CopyTargets as CopyTargets

-- | CR 707.10's own answer is ELIDED when the card does not print another, the
-- way CreateCopy elides a count of one: a copy carries the original's targets
-- unless the effect says otherwise, so @Copied@ is what most of the shape would
-- repeat.
codec :: Codec.Codec CopyStackObject.CopyStackObject
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec CopyStackObject.ref
  targets <- Fields.defaulted "targets" CopyTargets.defaultValue CopyTargets.codec CopyStackObject.targets
  pure
    CopyStackObject.MkCopyStackObject
      { CopyStackObject.ref = ref,
        CopyStackObject.targets = targets
      }
