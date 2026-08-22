{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryRestriction where

import qualified Pawl.Codec.Affected as Affected
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntryRestriction as EntryRestriction

-- | An object with two named keys, Pawl.Codec.SacrificeRestriction's shape plus
-- the origin zones. Untagged for that codec's reason: Pawl.Types.EntryRestriction
-- is a product with no sum for a tag to discriminate.
--
-- Both keys are REQUIRED. "origins" in particular has no defensible default: the
-- empty set would silently disarm the prohibition, and the full set would silently
-- widen a card that names two zones to all seven -- so a card that forgets the key
-- must fail to load rather than run wrong.
codec :: Codec.Codec EntryRestriction.EntryRestriction
codec = Fields.object $ do
  affected <- Fields.required "affected" Affected.codec EntryRestriction.affected
  origins <- Fields.required "origins" (Common.set Zone.codec) EntryRestriction.origins
  pure
    EntryRestriction.MkEntryRestriction
      { EntryRestriction.affected = affected,
        EntryRestriction.origins = origins
      }
