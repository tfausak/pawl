module Pawl.Codec.DrawRSpec where

import qualified Pawl.Codec.DrawR as DrawR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DrawR as DrawR
import qualified Pawl.Types.DrawRewrite as DrawRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DrawR" $ do
  -- CR 614.11 / 121.6: Words of Worship.
  Spec.it s "MkDrawR" $
    Common.assertCodec
      s
      DrawR.codec
      (DrawR.MkDrawR ControllerRelation.Yours (DrawRewrite.GainLife 5))
      " {\"whose\":{\"type\":\"Yours\"},\"rewrite\":{\"type\":\"GainLife\",\"value\":5}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DrawR.codec
