{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RandomCardInGraveyard where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.Codec.ZoneScope as ZoneScope
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RandomCardInGraveyard as RandomCardInGraveyard

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
--
-- @filter@ and @count@ are defaulted rather than required, so the unnarrowed
-- singular writes neither key; @players@ is required, since no scope is the
-- printed default.
codec :: Codec.Codec RandomCardInGraveyard.RandomCardInGraveyard
codec = Fields.object $ do
  players <- Fields.required "players" ZoneScope.codec RandomCardInGraveyard.players
  filter_ <- Fields.defaulted "filter" (Filter.And []) (Filter.codec Keyword.codec) RandomCardInGraveyard.filter
  count <- Fields.defaulted "count" (Quantity.Literal 1) Quantity.codec RandomCardInGraveyard.count
  pure
    RandomCardInGraveyard.MkRandomCardInGraveyard
      { RandomCardInGraveyard.players = players,
        RandomCardInGraveyard.filter = filter_,
        RandomCardInGraveyard.count = count
      }
