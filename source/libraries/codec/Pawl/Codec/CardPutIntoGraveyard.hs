{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CardPutIntoGraveyard where

import qualified Data.Set as Set
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CardPutIntoGraveyard as CardPutIntoGraveyard

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.CardLeavesZone is. The origin zones are defaulted, most printings
-- naming none.
codec :: Codec.Codec CardPutIntoGraveyard.CardPutIntoGraveyard
codec = Fields.object $ do
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) CardPutIntoGraveyard.filter
  from <- Fields.defaulted "from" Set.empty (Common.set Zone.codec) CardPutIntoGraveyard.from
  pure CardPutIntoGraveyard.MkCardPutIntoGraveyard {CardPutIntoGraveyard.filter = filter_, CardPutIntoGraveyard.from = from}
