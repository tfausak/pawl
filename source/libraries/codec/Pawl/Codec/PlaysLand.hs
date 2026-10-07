{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlaysLand where

import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlaysLand as PlaysLand

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.PermanentSacrificed is. Both keys are required: the zone is the
-- only narrowing a printing names, and "whenever you play a land" with none is
-- no card in the pool.
codec :: Codec.Codec PlaysLand.PlaysLand
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRelation.codec PlaysLand.player
  from <- Fields.required "from" Zone.codec PlaysLand.from
  pure PlaysLand.MkPlaysLand {PlaysLand.player = player, PlaysLand.from = from}
