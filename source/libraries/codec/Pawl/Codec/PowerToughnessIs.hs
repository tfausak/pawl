{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PowerToughnessIs where

import qualified Pawl.Codec.Reference as Reference
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PowerToughnessIs as PowerToughnessIs

codec :: Codec.Codec PowerToughnessIs.PowerToughnessIs
codec = Fields.object $ do
  object <- Fields.required "object" Reference.codec PowerToughnessIs.object
  power <- Fields.required "power" Common.integer PowerToughnessIs.power
  toughness <- Fields.required "toughness" Common.integer PowerToughnessIs.toughness
  pure PowerToughnessIs.MkPowerToughnessIs {PowerToughnessIs.object = object, PowerToughnessIs.power = power, PowerToughnessIs.toughness = toughness}
