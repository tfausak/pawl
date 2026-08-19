{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.TapForTotalPower where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464). The keyword codec is a PARAMETER; see
-- Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (TapForTotalPower.TapForTotalPower keyword)
codec keywordCodec = Fields.object $ do
  totalPower <- Fields.required "totalPower" Common.natural TapForTotalPower.totalPower
  whichPermanents <- Fields.required "whichPermanents" (Filter.codec keywordCodec) TapForTotalPower.whichPermanents
  pure TapForTotalPower.MkTapForTotalPower {TapForTotalPower.totalPower = totalPower, TapForTotalPower.whichPermanents = whichPermanents}
