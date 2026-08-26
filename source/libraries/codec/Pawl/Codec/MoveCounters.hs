{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MoveCounters where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MoveCounters as MoveCounters

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's MoveCounters arm.
--
-- `kind` defaults to Nothing, so the two-key form Agent's Toolkit writes stays
-- the wire spelling of "move a counter" with no kind named.
codec :: Codec.Codec MoveCounters.MoveCounters
codec = Fields.object $ do
  from <- Fields.required "from" SlotName.codec MoveCounters.from
  kind <- Fields.defaulted "kind" Nothing (Common.maybe (CounterKind.codec Keyword.codec)) MoveCounters.kind
  to <- Fields.required "to" SlotName.codec MoveCounters.to
  pure
    MoveCounters.MkMoveCounters
      { MoveCounters.from = from,
        MoveCounters.kind = kind,
        MoveCounters.to = to
      }
