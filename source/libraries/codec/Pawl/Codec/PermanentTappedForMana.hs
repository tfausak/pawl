{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PermanentTappedForMana where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaSpecification as ManaSpecification
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PermanentTappedForMana as PermanentTappedForMana

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentSacrificed is, and every key required for that codec's
-- reason: no part of the printed sentence has a default.
codec :: Codec.Codec PermanentTappedForMana.PermanentTappedForMana
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec PermanentTappedForMana.player
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PermanentTappedForMana.filter
  mana <- Fields.required "mana" ManaSpecification.codec PermanentTappedForMana.mana
  pure PermanentTappedForMana.MkPermanentTappedForMana {PermanentTappedForMana.player = player, PermanentTappedForMana.filter = filter_, PermanentTappedForMana.mana = mana}
