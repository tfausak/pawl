{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CopyStackObject where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CopyStackObject as CopyStackObject

-- | CR 707.10c's offer is ELIDED when the card does not print it, the way
-- CreateCopy elides a count of one: a copy carries the original's targets unless
-- the effect says otherwise, so @false@ is what most of the shape would repeat.
codec :: Codec.Codec CopyStackObject.CopyStackObject
codec = Fields.object $ do
  newTargets <- Fields.defaulted "newTargets" CopyStackObject.defaultNewTargets Common.boolean CopyStackObject.newTargets
  ref <- Fields.required "ref" ObjectRef.codec CopyStackObject.ref
  pure
    CopyStackObject.MkCopyStackObject
      { CopyStackObject.newTargets = newTargets,
        CopyStackObject.ref = ref
      }
