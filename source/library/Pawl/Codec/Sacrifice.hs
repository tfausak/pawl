{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Sacrifice where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Sacrifice as Sacrifice

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464). The keyword codec is a PARAMETER; see
-- Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (Sacrifice.Sacrifice keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural Sacrifice.count
  whichPermanents <- Fields.required "whichPermanents" (Filter.codec keywordCodec) Sacrifice.whichPermanents
  pure Sacrifice.MkSacrifice {Sacrifice.count = count, Sacrifice.whichPermanents = whichPermanents}
