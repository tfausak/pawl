{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExilePermanents where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExilePermanents as ExilePermanents

-- | A bare object keyed by the record's field names, Pawl.Codec.TapPermanents'
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ExilePermanents.ExilePermanents keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural ExilePermanents.count
  whichPermanents <- Fields.required "whichPermanents" (Filter.codec keywordCodec) ExilePermanents.whichPermanents
  pure ExilePermanents.MkExilePermanents {ExilePermanents.count = count, ExilePermanents.whichPermanents = whichPermanents}
