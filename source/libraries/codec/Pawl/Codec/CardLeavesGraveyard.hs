{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CardLeavesGraveyard where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CardLeavesGraveyard as CardLeavesGraveyard

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentSacrificed is. Both keys are required: a printing that
-- narrowed by neither would spell itself out as the trivial Filter and
-- TurnScope.EachTurn rather than leaving either out.
codec :: Codec.Codec CardLeavesGraveyard.CardLeavesGraveyard
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) CardLeavesGraveyard.filter
  scope <- Fields.required "scope" TurnScope.codec CardLeavesGraveyard.scope
  pure CardLeavesGraveyard.MkCardLeavesGraveyard {CardLeavesGraveyard.filter = filter_, CardLeavesGraveyard.scope = scope}
