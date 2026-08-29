{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttachBound where

import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttachBound as AttachBound

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's AttachBound arm.
codec :: Codec.Codec AttachBound.AttachBound
codec = Fields.object $ do
  subject <- Fields.required "subject" SlotName.codec AttachBound.subject
  destination <- Fields.required "destination" SlotName.codec AttachBound.destination
  pure
    AttachBound.MkAttachBound
      { AttachBound.subject = subject,
        AttachBound.destination = destination
      }
