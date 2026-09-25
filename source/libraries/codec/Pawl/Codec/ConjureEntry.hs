{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ConjureEntry where

import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ConjureEntry as ConjureEntry

-- | Both keys defaulted, Pawl.Codec.EntryRiders' spelling for the same two
-- riders, so a sentence states only what it prints.
codec :: Codec.Codec ConjureEntry.ConjureEntry
codec = Fields.object $ do
  tapped <- Fields.defaulted "tapped" (ConjureEntry.tapped ConjureEntry.defaultValue) TapState.codec ConjureEntry.tapped
  attacking <- Fields.defaulted "attacking" (ConjureEntry.attacking ConjureEntry.defaultValue) Common.boolean ConjureEntry.attacking
  pure ConjureEntry.MkConjureEntry {ConjureEntry.tapped = tapped, ConjureEntry.attacking = attacking}
