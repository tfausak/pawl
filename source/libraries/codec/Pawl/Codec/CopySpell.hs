{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CopySpell where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CopySpell as CopySpell

-- | CR 707.10c's offer is ELIDED when the card does not print it, the way
-- CreateCopy elides a count of one: a copy carries the original's targets unless
-- the effect says otherwise, so @false@ is what most of the shape would repeat.
codec :: Codec.Codec CopySpell.CopySpell
codec = Fields.object $ do
  newTargets <- Fields.defaulted "newTargets" CopySpell.defaultNewTargets Common.boolean CopySpell.newTargets
  ref <- Fields.required "ref" ObjectRef.codec CopySpell.ref
  pure
    CopySpell.MkCopySpell
      { CopySpell.newTargets = newTargets,
        CopySpell.ref = ref
      }
