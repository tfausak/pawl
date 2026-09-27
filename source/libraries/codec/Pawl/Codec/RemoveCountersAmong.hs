{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RemoveCountersAmong where

import qualified Data.Text as Text
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.RemovalCount as RemovalCount
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Codec.WhichCounters as WhichCounters
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RemovalCount as RemovalCount.Type
import qualified Pawl.Types.RemoveCountersAmong as RemoveCountersAmong
import qualified Pawl.Types.WhichCounters as WhichCounters.Type

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's RemoveCountersAmong arm. The tally slot is
-- elided when absent, Pawl.Codec.RemoveCounters' posture.
--
-- Counters of any kind are accepted only at "any number" (Eventide's Shadow):
-- no printing states a count over any kind, and the mixed-kind division prompt
-- has no cap to carry one ('Fields.objectWith''s check).
codec :: Codec.Codec RemoveCountersAmong.RemoveCountersAmong
codec = Fields.objectWith check $ do
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
  where
    check removal = case (RemoveCountersAmong.kind removal, RemoveCountersAmong.count removal) of
      (WhichCounters.Type.OfAnyKind, RemovalCount.Type.AnyNumber) -> Right removal
      (WhichCounters.Type.OfAnyKind, _) -> Left (Text.pack "RemoveCountersAmong: counters of any kind are removed only in any number")
      (WhichCounters.Type.OfKind _, _) -> Right removal
