module Pawl.Codec.LifeGainRSpec where

import qualified Pawl.Codec.LifeGainR as LifeGainR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.LifeGainR as LifeGainR
import qualified Pawl.Types.LifeGainRewrite as LifeGainRewrite
import qualified Pawl.Types.Scaling as Scaling

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.LifeGainR" $ do
  -- CR 614.1a / 119.10: Boon Reflection.
  Spec.it s "MkLifeGainR" $
    Common.assertCodec
      s
      LifeGainR.codec
      ( LifeGainR.MkLifeGainR
          ControllerRelation.Yours
          (LifeGainRewrite.Scaled (Scaling.Multiply 2))
      )
      " {\"whose\":{\"type\":\"Yours\"},\"rewrite\":{\"type\":\"Scaled\",\"value\":{\"type\":\"Multiply\",\"value\":2}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s LifeGainR.codec
