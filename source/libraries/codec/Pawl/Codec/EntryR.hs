{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EntryR where

import qualified Pawl.Codec.EntryRewrite as EntryRewrite
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EntryR as EntryR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec EntryR.EntryR
codec = Fields.object $ do
  matching <- Fields.required "matching" (Filter.codec Keyword.codec) EntryR.matching
  rewrite <- Fields.required "rewrite" EntryRewrite.codec EntryR.rewrite
  pure
    EntryR.MkEntryR
      { EntryR.matching = matching,
        EntryR.rewrite = rewrite
      }
