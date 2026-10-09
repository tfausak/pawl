module Pawl.Codec.ScryRSpec where

import qualified Pawl.Codec.ScryR as ScryR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ScryR as ScryR
import qualified Pawl.Types.ScryRewrite as ScryRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ScryR" $ do
  -- CR 701.22a: Kenessos, Priest of Thassa.
  Spec.it s "MkScryR" $
    Common.assertCodec
      s
      ScryR.codec
      (ScryR.MkScryR (ControllerRelation.Related PlayerRelation.You) ScryRewrite.PlusOne)
      " {\"whose\":{\"type\":\"You\"},\"rewrite\":{\"type\":\"PlusOne\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ScryR.codec
