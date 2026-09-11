{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CopySnapshot where

import qualified Pawl.Codec.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CopySnapshot as CopySnapshot

-- | Runtime-only, never in card JSON: both readings of one layer-1 copy result.
codec :: Codec.Codec CopySnapshot.CopySnapshot
codec = Fields.object $ do
  normal <- Fields.required "normal" ProjectedCharacteristics.codec CopySnapshot.normal
  flipped <- Fields.defaulted "flipped" Nothing (Common.maybe ProjectedCharacteristics.codec) CopySnapshot.flipped
  pure
    CopySnapshot.MkCopySnapshot
      { CopySnapshot.normal = normal,
        CopySnapshot.flipped = flipped
      }
