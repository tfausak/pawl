module Pawl.Codec.InitiativeTargetSpec where

import qualified Pawl.Codec.InitiativeTarget as InitiativeTarget
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.InitiativeTarget as InitiativeTarget

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.InitiativeTarget" $ do
  Spec.it s "TheController" $
    Common.assertCodec
      s
      InitiativeTarget.codec
      InitiativeTarget.TheController
      " {\"type\":\"TheController\"} "
  Spec.it s "ControllerOfSource" $
    Common.assertCodec
      s
      InitiativeTarget.codec
      InitiativeTarget.ControllerOfSource
      " {\"type\":\"ControllerOfSource\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s InitiativeTarget.codec
