module Pawl.Codec.ProliferateRSpec where

import qualified Pawl.Codec.ProliferateR as ProliferateR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProliferateR as ProliferateR
import qualified Pawl.Types.ProliferateRewrite as ProliferateRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ProliferateR" $ do
  -- CR 701.34a: Tekuthal, Inquiry Dominus.
  Spec.it s "MkProliferateR" $
    Common.assertCodec
      s
      ProliferateR.codec
      (ProliferateR.MkProliferateR (ControllerRelation.Related PlayerRelation.You) ProliferateRewrite.Doubled)
      " {\"whose\":{\"type\":\"You\"},\"rewrite\":{\"type\":\"Doubled\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ProliferateR.codec
