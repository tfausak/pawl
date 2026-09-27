{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CountersFromPermanents where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.CounterSpread as CounterSpread
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.WhichCounters as WhichCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CounterSpread as CounterSpread.Type
import qualified Pawl.Types.CountersFromPermanents as CountersFromPermanents

-- | A bare object keyed by the record's field names, Pawl.Codec.TapPermanents'
-- shape. The keyword codec is a PARAMETER; see Pawl.Codec.Filter's header.
-- The spread is omitted at FromOne, the one-permanent form.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (CountersFromPermanents.CountersFromPermanents keyword)
codec keywordCodec = Fields.object $ do
  count <- Fields.required "count" Common.natural CountersFromPermanents.count
  kind <- Fields.required "kind" (WhichCounters.codec keywordCodec) CountersFromPermanents.kind
  whichPermanent <- Fields.required "whichPermanent" (Filter.codec keywordCodec) CountersFromPermanents.whichPermanent
  spread <- Fields.defaulted "spread" CounterSpread.Type.FromOne CounterSpread.codec CountersFromPermanents.spread
  pure CountersFromPermanents.MkCountersFromPermanents {CountersFromPermanents.count = count, CountersFromPermanents.kind = kind, CountersFromPermanents.whichPermanent = whichPermanent, CountersFromPermanents.spread = spread}
