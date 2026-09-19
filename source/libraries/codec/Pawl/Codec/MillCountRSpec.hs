module Pawl.Codec.MillCountRSpec where

import qualified Pawl.Codec.MillCountR as MillCountR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.MillCountR as MillCountR
import qualified Pawl.Types.MillCountRewrite as MillCountRewrite
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.MillCountR" $ do
  -- CR 614.1a / 701.17a: Bruvac the Grandiloquent.
  Spec.it s "MkMillCountR" $
    Common.assertCodec
      s
      MillCountR.codec
      ( MillCountR.MkMillCountR
          ControllerRelation.Opponents
          (MillCountRewrite.Scaled (Scaling.Multiply 2))
      )
      " {\"whose\":{\"type\":\"Opponents\"},\"rewrite\":{\"type\":\"Scaled\",\"value\":{\"type\":\"Multiply\",\"value\":2}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s MillCountR.codec
