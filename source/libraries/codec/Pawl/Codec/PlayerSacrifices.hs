{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerSacrifices where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerSacrifices as PlayerSacrifices

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's PlayerSacrifices arm.
codec :: Codec.Codec PlayerSacrifices.PlayerSacrifices
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec PlayerSacrifices.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PlayerSacrifices.filter
  quantity <- Fields.required "quantity" Quantity.codec PlayerSacrifices.quantity
  pure
    PlayerSacrifices.MkPlayerSacrifices
      { PlayerSacrifices.slot = slot,
        PlayerSacrifices.filter = filter_,
        PlayerSacrifices.quantity = quantity
      }
