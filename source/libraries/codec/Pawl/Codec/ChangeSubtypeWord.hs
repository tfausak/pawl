{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ChangeSubtypeWord where

import qualified Pawl.Codec.Subtype as Subtype
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ChangeSubtypeWord as ChangeSubtypeWord

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec ChangeSubtypeWord.ChangeSubtypeWord
codec = Fields.object $ do
  from_ <- Fields.required "from" Subtype.codec ChangeSubtypeWord.from
  to <- Fields.required "to" Subtype.codec ChangeSubtypeWord.to
  pure
    ChangeSubtypeWord.MkChangeSubtypeWord
      { ChangeSubtypeWord.from = from_,
        ChangeSubtypeWord.to = to
      }
