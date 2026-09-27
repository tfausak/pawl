{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CardsPutIntoZone where

import qualified Data.Set as Set
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CardsPutIntoZone as CardsPutIntoZone

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.CardPutIntoGraveyard is. The origin zones are defaulted, "from
-- anywhere" naming none.
codec :: Codec.Codec CardsPutIntoZone.CardsPutIntoZone
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) CardsPutIntoZone.filter
  from <- Fields.defaulted "from" Set.empty (Common.set Zone.codec) CardsPutIntoZone.from
  to <- Fields.required "to" Zone.codec CardsPutIntoZone.to
  pure CardsPutIntoZone.MkCardsPutIntoZone {CardsPutIntoZone.filter = filter_, CardsPutIntoZone.from = from, CardsPutIntoZone.to = to}
