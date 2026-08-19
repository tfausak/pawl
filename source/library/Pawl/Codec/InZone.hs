{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.InZone where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.InZone as InZone

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec InZone.InZone
codec = Fields.object $ do
  zone <- Fields.required "zone" Zone.codec InZone.zone
  player <- Fields.required "player" PlayerRef.codec InZone.player
  pure InZone.MkInZone {InZone.zone = zone, InZone.player = player}
