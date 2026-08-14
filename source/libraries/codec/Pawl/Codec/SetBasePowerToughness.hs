{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.SetBasePowerToughness where

import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.SetBasePowerToughness as SetBasePowerToughness

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec SetBasePowerToughness.SetBasePowerToughness
codec = Fields.object $ do
  power <- Fields.required "power" Quantity.codec SetBasePowerToughness.power
  toughness <- Fields.required "toughness" Quantity.codec SetBasePowerToughness.toughness
  pure
    SetBasePowerToughness.MkSetBasePowerToughness
      { SetBasePowerToughness.power = power,
        SetBasePowerToughness.toughness = toughness
      }
