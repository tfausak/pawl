{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttachTarget where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttachTarget as AttachTarget

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's AttachTarget arm.
codec :: Codec.Codec AttachTarget.AttachTarget
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec AttachTarget.slot
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) AttachTarget.filter
  pure
    AttachTarget.MkAttachTarget
      { AttachTarget.slot = slot,
        AttachTarget.filter = filter_
      }
