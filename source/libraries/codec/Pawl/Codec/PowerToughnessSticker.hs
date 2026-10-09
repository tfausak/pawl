{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PowerToughnessSticker where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PowerToughnessSticker as PowerToughnessSticker

-- | A bare object keyed by the record's field names.
codec :: Codec.Codec PowerToughnessSticker.PowerToughnessSticker
codec = Fields.object $ do
  tickets <- Fields.required "tickets" Common.natural PowerToughnessSticker.tickets
  power <- Fields.required "power" Common.integer PowerToughnessSticker.power
  toughness <- Fields.required "toughness" Common.integer PowerToughnessSticker.toughness
  pure
    PowerToughnessSticker.MkPowerToughnessSticker
      { PowerToughnessSticker.tickets = tickets,
        PowerToughnessSticker.power = power,
        PowerToughnessSticker.toughness = toughness
      }
