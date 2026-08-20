{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChosenCardFromAmong where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChosenCardFromAmong as ChosenCardFromAmong

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
codec :: Codec.Codec ChosenCardFromAmong.ChosenCardFromAmong
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec ChosenCardFromAmong.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) ChosenCardFromAmong.filter
  pure
    ChosenCardFromAmong.MkChosenCardFromAmong
      { ChosenCardFromAmong.slot = slot,
        ChosenCardFromAmong.filter = filter_
      }
