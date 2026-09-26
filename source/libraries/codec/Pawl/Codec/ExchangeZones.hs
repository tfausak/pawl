{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ExchangeZones where

import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.ZonePair as ZonePair
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ExchangeZones as ExchangeZones

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec ExchangeZones.ExchangeZones
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec ExchangeZones.player
  zones <- Fields.required "zones" ZonePair.codec ExchangeZones.zones
  pure
    ExchangeZones.MkExchangeZones
      { ExchangeZones.player = player,
        ExchangeZones.zones = zones
      }
