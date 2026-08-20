{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.EachCardInGraveyard where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.GraveyardScope as GraveyardScope
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.EachCardInGraveyard as EachCardInGraveyard

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec EachCardInGraveyard.EachCardInGraveyard
codec = Fields.object $ do
  graveyards <- Fields.required "graveyards" GraveyardScope.codec EachCardInGraveyard.graveyards
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) EachCardInGraveyard.filter
  pure
    EachCardInGraveyard.MkEachCardInGraveyard
      { EachCardInGraveyard.graveyards = graveyards,
        EachCardInGraveyard.filter = filter_
      }
