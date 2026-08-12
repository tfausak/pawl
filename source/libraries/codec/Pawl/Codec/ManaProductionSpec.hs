{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ManaProductionSpec where

import qualified Pawl.Codec.ManaProduction as ManaProduction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.ManaProduction as ManaProduction
import qualified Pawl.Types.ManaType as ManaType

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaProduction" $ do
  Spec.it s "OfType" $
    Common.assertJsonCodec
      s
      ManaProduction.toJson
      ManaProduction.fromJson
      (ManaProduction.OfType (ManaType.Colored Color.Green))
      """ {"type":"OfType","value":{"type":"Colored","value":{"type":"Green"}}} """
  Spec.it s "AnyColor" $
    Common.assertJsonCodec
      s
      ManaProduction.toJson
      ManaProduction.fromJson
      ManaProduction.AnyColor
      """ {"type":"AnyColor"} """
  Spec.it s "Chosen" $
    Common.assertJsonCodec
      s
      ManaProduction.toJson
      ManaProduction.fromJson
      ManaProduction.Chosen
      """ {"type":"Chosen"} """
  Spec.it s "SnowSymbol" $
    Common.assertJsonCodec
      s
      ManaProduction.toJson
      ManaProduction.fromJson
      ManaProduction.SnowSymbol
      """ {"type":"SnowSymbol"} """
