{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.RandomCardInHand where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RandomCardInHand as RandomCardInHand

-- | A bare object keyed by the record's field names, the shape every other
-- 'Pawl.Types.ObjectRef' payload record takes.
--
-- @filter@ and @count@ are defaulted rather than required, so the unnarrowed
-- singular -- Merfolk Spy's and Wild Evocation's "a card at random from their
-- hand" -- writes neither key.
codec :: Codec.Codec RandomCardInHand.RandomCardInHand
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec RandomCardInHand.player
  filter_ <- Fields.defaulted "filter" (Filter.And []) (Filter.codec Keyword.codec) RandomCardInHand.filter
  count <- Fields.defaulted "count" (Quantity.Literal 1) Quantity.codec RandomCardInHand.count
  pure
    RandomCardInHand.MkRandomCardInHand
      { RandomCardInHand.player = player,
        RandomCardInHand.filter = filter_,
        RandomCardInHand.count = count
      }
