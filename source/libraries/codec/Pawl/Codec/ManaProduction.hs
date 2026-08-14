module Pawl.Codec.ManaProduction where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.Types.ManaProduction as ManaProduction

codec :: Codec.Codec ManaProduction.ManaProduction
codec =
  Arm.tagged
    [ Arm.payload "OfType" ManaType.codec ManaProduction.OfType (\x -> case x of ManaProduction.OfType y -> Just y; _ -> Nothing),
      Arm.nullary "AnyColor" ManaProduction.AnyColor,
      Arm.nullary "Chosen" ManaProduction.Chosen,
      Arm.nullary "SnowSymbol" ManaProduction.SnowSymbol
    ]
