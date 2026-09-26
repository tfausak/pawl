{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.KeywordTally where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.KeywordTally as KeywordTally

-- | A bare object keyed by the record's field names, Pawl.Codec.Count's shape
-- less its aggregation. The keyword codec is a PARAMETER; see
-- Pawl.Codec.Filter's header.
codec :: (Typeable.Typeable keyword, Eq keyword) => Codec.Codec keyword -> Codec.Codec (KeywordTally.KeywordTally keyword)
codec keywordCodec = Fields.object $ do
  scope <- Fields.required "scope" Scope.codec KeywordTally.scope
  filter_ <- Fields.required "filter" (Filter.codec keywordCodec) KeywordTally.filter
  pure KeywordTally.MkKeywordTally {KeywordTally.scope = scope, KeywordTally.filter = filter_}
