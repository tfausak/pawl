{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ExtraPhaseSpec where

import qualified Pawl.Codec.ExtraPhase as ExtraPhase
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExtraPhase as ExtraPhase

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExtraPhase" $ do
  Spec.it s "ExtraCombat" $
    Common.assertCodec
      s
      ExtraPhase.codec
      ExtraPhase.ExtraCombat
      """ {"type":"ExtraCombat"} """
  Spec.it s "ExtraMain" $
    Common.assertCodec
      s
      ExtraPhase.codec
      ExtraPhase.ExtraMain
      """ {"type":"ExtraMain"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s ExtraPhase.codec
