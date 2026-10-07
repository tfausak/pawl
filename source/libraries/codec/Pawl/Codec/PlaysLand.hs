{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlaysLand where

import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlaysLand as PlaysLand

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentSacrificed is. Every key is required: "whenever you play
-- a land" with no zone is no card in the pool, and the unrestricted land spells
-- itself out as the trivial Filter.
codec :: Codec.Codec PlaysLand.PlaysLand
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec PlaysLand.player
  from <- Fields.required "from" Zone.codec PlaysLand.from
  filter_ <- Fields.required "filter" (Filter.codec Keyword.codec) PlaysLand.filter
  pure PlaysLand.MkPlaysLand {PlaysLand.player = player, PlaysLand.from = from, PlaysLand.filter = filter_}
