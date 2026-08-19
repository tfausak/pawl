{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TapPermanents where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TapPermanents as TapPermanents

-- | A bare object keyed by the record's field names, Pawl.Codec.Sacrifice's
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (TapPermanents.TapPermanents keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural TapPermanents.count
  whichPermanents <- Fields.required "whichPermanents" (Filter.codec keywordCodec) TapPermanents.whichPermanents
  pure TapPermanents.MkTapPermanents {TapPermanents.count = count, TapPermanents.whichPermanents = whichPermanents}
