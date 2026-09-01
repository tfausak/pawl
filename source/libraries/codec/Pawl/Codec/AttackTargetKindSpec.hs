module Pawl.Codec.AttackTargetKindSpec where

import qualified Pawl.Codec.AttackTargetKind as AttackTargetKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttackTargetKind" $ do
  Spec.it s "OfPlayer" $
    Common.assertCodec
      s
      AttackTargetKind.codec
      AttackTargetKind.OfPlayer
      " {\"type\":\"OfPlayer\"} "
  Spec.it s "OfPlaneswalker" $
    Common.assertCodec
      s
      AttackTargetKind.codec
      AttackTargetKind.OfPlaneswalker
      " {\"type\":\"OfPlaneswalker\"} "
  Spec.it s "OfBattle" $
    Common.assertCodec
      s
      AttackTargetKind.codec
      AttackTargetKind.OfBattle
      " {\"type\":\"OfBattle\"} "
  -- PlayerScopeSpec's posture: Arm.enum derives the arm list from the type, so
  -- this is what would catch a constructor the derivation missed.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s AttackTargetKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s AttackTargetKind.codec
