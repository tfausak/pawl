{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.KeywordsAre where

import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.KeywordsAre as KeywordsAre

-- | The keyword is spelled as a card's JSON spells it.
codec :: Codec.Codec KeywordsAre.KeywordsAre
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec KeywordsAre.object
  keyword <- Fields.required "keyword" Keyword.codec KeywordsAre.keyword
  count <- Fields.required "count" Common.natural KeywordsAre.count
  pure KeywordsAre.MkKeywordsAre {KeywordsAre.object = object, KeywordsAre.keyword = keyword, KeywordsAre.count = count}
