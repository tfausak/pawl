{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Devour where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.DevourCount as DevourCount
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Devour as Devour

-- | A bare object keyed by the record's field names. The keyword codec is a
-- PARAMETER; see Pawl.Codec.Filter's header.
--
-- The quality is elided when absent, which is what every CR 702.82a printing
-- writes; the count is required, rule 702.82a stating an N in every variant.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Devour.Devour keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" DevourCount.codec Devour.count
  quality <- Fields.defaulted "quality" Nothing (Common.maybe (Filter.codec keywordCodec)) Devour.quality
  pure Devour.MkDevour {Devour.count = count, Devour.quality = quality}
