{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RemoveCounters where

import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.RemoveCounters as RemoveCounters

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's RemoveCounters arm.
codec :: Codec.Codec RemoveCounters.RemoveCounters
codec = Fields.object $ do
  kind <- Fields.required "kind" (CounterKind.codec Keyword.codec) RemoveCounters.kind
  quantity <- Fields.required "quantity" Quantity.codec RemoveCounters.quantity
  slot <- Fields.required "slot" SlotName.codec RemoveCounters.slot
  pure
    RemoveCounters.MkRemoveCounters
      { RemoveCounters.kind = kind,
        RemoveCounters.quantity = quantity,
        RemoveCounters.slot = slot
      }
