{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttachedToBound where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttachedToBound as AttachedToBound

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.ObjectRef's AttachedToBound arm.
codec :: Codec.Codec AttachedToBound.AttachedToBound
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec AttachedToBound.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) AttachedToBound.filter
  pure
    AttachedToBound.MkAttachedToBound
      { AttachedToBound.slot = slot,
        AttachedToBound.filter = filter_
      }
