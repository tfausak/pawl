{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CharacteristicPT where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CharacteristicPT as CharacteristicPT

-- | A bare object keyed by the record's field names, replacing the
-- @[power, toughness]@ array this payload used to be. Runtime-only:
-- ProjectedCharacteristics serialises a projection, never card data.
codec :: Codec.Codec CharacteristicPT.CharacteristicPT
codec = Fields.object $ do
  power <- Fields.required "power" Quantity.codec CharacteristicPT.power
  toughness <- Fields.required "toughness" Quantity.codec CharacteristicPT.toughness
  pure
    CharacteristicPT.MkCharacteristicPT
      { CharacteristicPT.power = power,
        CharacteristicPT.toughness = toughness
      }
