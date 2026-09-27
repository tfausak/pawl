{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RemoveCountersAmong where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.RemovalCount as RemovalCount
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.WhichCounters as WhichCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RemoveCountersAmong as RemoveCountersAmong

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's RemoveCountersAmong arm. The tally slot is
-- elided when absent, Pawl.Codec.RemoveCounters' posture.
codec :: Codec.Codec RemoveCountersAmong.RemoveCountersAmong
codec = Fields.object $ do
  count <- Fields.required "count" RemovalCount.codec RemoveCountersAmong.count
  from <- Fields.required "from" ObjectRef.codec RemoveCountersAmong.from
  kind <- Fields.required "kind" (WhichCounters.codec Keyword.codec) RemoveCountersAmong.kind
  tally <- Fields.defaulted "tally" Nothing (Common.maybe SlotName.codec) RemoveCountersAmong.tally
  pure
    RemoveCountersAmong.MkRemoveCountersAmong
      { RemoveCountersAmong.count = count,
        RemoveCountersAmong.from = from,
        RemoveCountersAmong.kind = kind,
        RemoveCountersAmong.tally = tally
      }
