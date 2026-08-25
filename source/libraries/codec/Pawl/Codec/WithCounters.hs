module Pawl.Codec.WithCounters where

import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.WithCounters as WithCounters

-- | An array of kind/count pairs ascending by kind, which is EntryRiders'
-- counter spelling reused rather than re-spelled: the two payloads say the same
-- thing about the same rule (CR 614.1c and CR 122.6), so one wire form serves
-- both. It replaced the single @{"kind":...,"amount":...}@ object when the row
-- grew to several kinds (#2314), which itself replaced a two-element array
-- (#1464).
--
-- 'Common.nonEmptyKeyedList' and not 'Common.keyedList': an empty array would be
-- a row that places nothing, which no card prints.
codec :: Codec.Codec WithCounters.WithCounters
codec =
  Codec.MkCodec
    { Codec.encode = Codec.encode inner . WithCounters.counters,
      Codec.decode = fmap WithCounters.MkWithCounters . Codec.decode inner,
      Codec.schema = Codec.schema inner
    }
  where
    inner = Common.nonEmptyKeyedList EntryRiders.counter
