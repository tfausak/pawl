{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChoosePermanents where

import qualified Pawl.Codec.ChosenPermanents as ChosenPermanents
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChoosePermanents as ChoosePermanents

-- | A bare object keyed by the record's field names, Pawl.Codec.AnyNumberDiscard's
-- shape.
codec :: Codec.Codec ChoosePermanents.ChoosePermanents
codec = Fields.object $ do
  permanents <- Fields.required "permanents" ChosenPermanents.codec ChoosePermanents.permanents
  slot <- Fields.required "slot" SlotName.codec ChoosePermanents.slot
  pure
    ChoosePermanents.MkChoosePermanents
      { ChoosePermanents.permanents = permanents,
        ChoosePermanents.slot = slot
      }
