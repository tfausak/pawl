{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecomeCopy where

import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecomeCopy as BecomeCopy

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's BecomeCopy arm.
codec :: Codec.Codec BecomeCopy.BecomeCopy
codec = Fields.object $ do
  original <- Fields.required "original" ObjectRef.codec BecomeCopy.original
  subject <- Fields.required "subject" ObjectRef.codec BecomeCopy.subject
  pure
    BecomeCopy.MkBecomeCopy
      { BecomeCopy.original = original,
        BecomeCopy.subject = subject
      }
