{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RemovePlusOneCounters where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterSpread as CounterSpread
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterSpread as CounterSpread.Type
import qualified Pawl.Types.RemovePlusOneCounters as RemovePlusOneCounters

-- | A bare object keyed by the record's field names, Pawl.Codec.TapPermanents'
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
-- The spread is omitted at FromOne, the one-permanent form.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (RemovePlusOneCounters.RemovePlusOneCounters keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural RemovePlusOneCounters.count
  whichPermanent <- Fields.required "whichPermanent" (Filter.codec keywordCodec) RemovePlusOneCounters.whichPermanent
  spread <- Fields.defaulted "spread" CounterSpread.Type.FromOne CounterSpread.codec RemovePlusOneCounters.spread
  pure RemovePlusOneCounters.MkRemovePlusOneCounters {RemovePlusOneCounters.count = count, RemovePlusOneCounters.whichPermanent = whichPermanent, RemovePlusOneCounters.spread = spread}
