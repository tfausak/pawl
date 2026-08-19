{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ModifyPowerToughness where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ModifyPowerToughness as ModifyPowerToughness

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec ModifyPowerToughness.ModifyPowerToughness
codec = Fields.object $ do
  power <- Fields.required "power" Quantity.codec ModifyPowerToughness.power
  toughness <- Fields.required "toughness" Quantity.codec ModifyPowerToughness.toughness
  pure
    ModifyPowerToughness.MkModifyPowerToughness
      { ModifyPowerToughness.power = power,
        ModifyPowerToughness.toughness = toughness
      }
