module Pawl.Codec.ManaProduction where

import qualified Pawl.Codec.ManaType as ManaType
import qualified Pawl.JsonCodec.Arm as Arm
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaProduction as ManaProduction

codec :: Codec.Codec ManaProduction.ManaProduction
codec =
  Arm.tagged
    encode
    [ Arm.payload "OfType" ManaType.codec ManaProduction.OfType,
      Arm.nullary "AnyColor" ManaProduction.AnyColor,
      Arm.nullary "Chosen" ManaProduction.Chosen,
      Arm.nullary "SnowSymbol" ManaProduction.SnowSymbol
    ]
  where
    encode mp = case mp of
      ManaProduction.OfType mt -> Common.tagged "OfType" . Just $ Codec.encode ManaType.codec mt
      ManaProduction.AnyColor -> Common.nullary "AnyColor"
      ManaProduction.Chosen -> Common.nullary "Chosen"
      ManaProduction.SnowSymbol -> Common.nullary "SnowSymbol"
