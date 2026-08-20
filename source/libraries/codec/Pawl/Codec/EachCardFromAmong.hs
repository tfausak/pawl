{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EachCardFromAmong where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EachCardFromAmong as EachCardFromAmong

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes -- and the same two keys
-- 'Pawl.Codec.ChosenCardFromAmong' writes, this type being its plural.
codec :: Codec.Codec EachCardFromAmong.EachCardFromAmong
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec EachCardFromAmong.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) EachCardFromAmong.filter
  pure
    EachCardFromAmong.MkEachCardFromAmong
      { EachCardFromAmong.slot = slot,
        EachCardFromAmong.filter = filter_
      }
