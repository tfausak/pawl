{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ReturnPermanents where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents

-- | A bare object keyed by the record's field names, Pawl.Codec.TapPermanents'
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (ReturnPermanents.ReturnPermanents keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural ReturnPermanents.count
  whichPermanents <- Fields.required "whichPermanents" (Filter.codec keywordCodec) ReturnPermanents.whichPermanents
  pure ReturnPermanents.MkReturnPermanents {ReturnPermanents.count = count, ReturnPermanents.whichPermanents = whichPermanents}
