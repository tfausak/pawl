{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MoveCounters where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MoveCounters as MoveCounters

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's MoveCounters arm.
codec :: Codec.Codec MoveCounters.MoveCounters
codec = Fields.object $ do
  from <- Fields.required "from" SlotName.codec MoveCounters.from
  to <- Fields.required "to" SlotName.codec MoveCounters.to
  pure
    MoveCounters.MkMoveCounters
      { MoveCounters.from = from,
        MoveCounters.to = to
      }
