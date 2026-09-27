{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AttachAll where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AttachAll as AttachAll

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's AttachAll arm.
codec :: Codec.Codec AttachAll.AttachAll
codec = Fields.object $ do
  subjects <- Fields.required "subjects" ObjectRef.codec AttachAll.subjects
  destination <- Fields.required "destination" (Filter.codec Keyword.codec) AttachAll.destination
  pure
    AttachAll.MkAttachAll
      { AttachAll.subjects = subjects,
        AttachAll.destination = destination
      }
