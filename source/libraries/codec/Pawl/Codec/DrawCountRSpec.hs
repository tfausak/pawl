module Pawl.Codec.DrawCountRSpec where

import qualified Pawl.Codec.DrawCountR as DrawCountR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DrawCountR as DrawCountR
import qualified Pawl.Types.DrawCountRewrite as DrawCountRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DrawCountR" $ do
  -- CR 121.2a: Alms Collector.
  Spec.it s "MkDrawCountR" $
    Common.assertCodec
      s
      DrawCountR.codec
      (DrawCountR.MkDrawCountR ControllerRelation.Opponents 2 DrawCountRewrite.EachDrawOne)
      " {\"whose\":{\"type\":\"Opponents\"},\"atLeast\":2,\"rewrite\":{\"type\":\"EachDrawOne\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DrawCountR.codec
