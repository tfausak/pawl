{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChosenCardFromAmong where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
--
-- @count@ and @chooser@ are defaulted rather than required, so the printed
-- singular addressed to the resolving controller -- which is every "from among
-- them" but Ancestral Memories' and Animal Magnetism's -- writes neither key.
codec :: Codec.Codec ChosenCardFromAmong.ChosenCardFromAmong
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec ChosenCardFromAmong.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenCardFromAmong.filter
  count <- Fields.defaulted "count" (Quantity.Literal 1) Quantity.codec ChosenCardFromAmong.count
  chooser <- Fields.defaulted "chooser" (PlayerRef.Relative PlayerRelation.You) PlayerRef.codec ChosenCardFromAmong.chooser
  pure
    ChosenCardFromAmong.MkChosenCardFromAmong
      { ChosenCardFromAmong.slot = slot,
        ChosenCardFromAmong.filter = filter_,
        ChosenCardFromAmong.count = count,
        ChosenCardFromAmong.chooser = chooser
      }
