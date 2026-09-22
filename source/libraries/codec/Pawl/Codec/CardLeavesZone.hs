{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CardLeavesZone where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CardLeavesZone as CardLeavesZone

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentSacrificed is. The filter, the scope and the zone left are
-- required: a printing that narrowed by neither of the first two would spell
-- itself out as the trivial Filter and TurnScope.EachTurn rather than leaving
-- either out. The destination is defaulted, most printings naming none.
codec :: Codec.Codec CardLeavesZone.CardLeavesZone
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) CardLeavesZone.filter
  scope <- Fields.required "scope" TurnScope.codec CardLeavesZone.scope
  from <- Fields.required "from" Zone.codec CardLeavesZone.from
  to <- Fields.defaulted "to" Nothing (Common.maybe Zone.codec) CardLeavesZone.to
  pure CardLeavesZone.MkCardLeavesZone {CardLeavesZone.filter = filter_, CardLeavesZone.scope = scope, CardLeavesZone.from = from, CardLeavesZone.to = to}
