{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RandomCardInLibrary where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RandomCardInLibrary as RandomCardInLibrary

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
--
-- @filter@ and @count@ are defaulted rather than required, so the unnarrowed
-- singular writes neither key, and Gate to Seatower's "seek a nonland card"
-- writes its filter alone.
codec :: Codec.Codec RandomCardInLibrary.RandomCardInLibrary
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec RandomCardInLibrary.player
  filter_ <- Fields.defaulted "filter" (Filter.And []) (Filter.codec Keyword.codec) RandomCardInLibrary.filter
  count <- Fields.defaulted "count" (Quantity.Literal 1) Quantity.codec RandomCardInLibrary.count
  pure
    RandomCardInLibrary.MkRandomCardInLibrary
      { RandomCardInLibrary.player = player,
        RandomCardInLibrary.filter = filter_,
        RandomCardInLibrary.count = count
      }
