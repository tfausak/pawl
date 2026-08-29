{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MoveCounters where

import qualified Pawl.Codec.MovedKinds as MovedKinds
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MoveCounters as MoveCounters
import qualified Pawl.Types.MovedKinds as MovedKinds.Type
import qualified Pawl.Types.Quantity as Quantity

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's MoveCounters arm.
--
-- `kinds` defaults to one kind the player picks and a count of one, so the
-- two-key form Agent's Toolkit writes stays the wire spelling of "move a
-- counter" with no kind named. `slot` defaults to Nothing for the same reason:
-- that is what "move a counter" with nothing looking back at it means, so a card
-- that says only that writes neither key.
codec :: Codec.Codec MoveCounters.MoveCounters
codec = Fields.object $ do
  from <- Fields.required "from" SlotName.codec MoveCounters.from
  kinds <- Fields.defaulted "kinds" (MovedKinds.Type.Chosen (Quantity.Literal 1)) MovedKinds.codec MoveCounters.kinds
  slot <- Fields.defaulted "slot" Nothing (Common.maybe SlotName.codec) MoveCounters.slot
  to <- Fields.required "to" SlotName.codec MoveCounters.to
  pure
    MoveCounters.MkMoveCounters
      { MoveCounters.from = from,
        MoveCounters.kinds = kinds,
        MoveCounters.slot = slot,
        MoveCounters.to = to
      }
