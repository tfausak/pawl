{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.MillTally where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.MillTally as MillTally

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes. Pawl.Codec.Mill is the only
-- caller, and it is what says the value is a mill's tally.
codec :: Codec.Codec MillTally.MillTally
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec MillTally.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) MillTally.filter
  pure
    MillTally.MkMillTally
      { MillTally.slot = slot,
        MillTally.filter = filter_
      }
