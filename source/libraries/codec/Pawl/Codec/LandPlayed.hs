{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LandPlayed where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LandPlayed as LandPlayed

-- | A bare object keyed by the record's field names, as
-- Pawl.Codec.LeftTheGame is. Every key is required.
codec :: Codec.Codec LandPlayed.LandPlayed
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec LandPlayed.player
  land <- Fields.required "land" ObjectId.codec LandPlayed.land
  from <- Fields.required "from" Zone.codec LandPlayed.from
  pure LandPlayed.MkLandPlayed {LandPlayed.player = player, LandPlayed.land = land, LandPlayed.from = from}
