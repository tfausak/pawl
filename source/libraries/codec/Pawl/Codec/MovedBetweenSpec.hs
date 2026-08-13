{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.MovedBetweenSpec where

import qualified Pawl.Codec.MovedBetween as MovedBetween
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.MovedBetween as MovedBetween
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MovedBetween" $ do
  -- BOTH keys are a Zone, so the fixture names them differently on purpose:
  -- only an asymmetric case catches a codec that matched the move backwards.
  Spec.it s "MkMovedBetween, both keys" $
    Common.assertCodec
      s
      MovedBetween.codec
      (MovedBetween.MkMovedBetween {MovedBetween.from = Zone.Battlefield, MovedBetween.to = Zone.Graveyard})
      """ {"from":{"type":"Battlefield"},"to":{"type":"Graveyard"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s MovedBetween.codec
