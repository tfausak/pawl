module Pawl.Codec.DieRollRSpec where

import qualified Pawl.Codec.DieRollR as DieRollR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.DieRollR as DieRollR
import qualified Pawl.Types.DieRollRewrite as DieRollRewrite
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DieRollR" $ do
  -- CR 706.6: Pixie Guide.
  Spec.it s "MkDieRollR" $
    Common.assertCodec
      s
      DieRollR.codec
      (DieRollR.MkDieRollR (ControllerRelation.Related PlayerRelation.You) DieRollRewrite.ExtraIgnoringLowest)
      " {\"whose\":{\"type\":\"You\"},\"rewrite\":{\"type\":\"ExtraIgnoringLowest\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DieRollR.codec
