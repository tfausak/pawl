module Pawl.Codec.VillainousChoiceRSpec where

import qualified Pawl.Codec.VillainousChoiceR as VillainousChoiceR
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.VillainousChoiceR as VillainousChoiceR
import qualified Pawl.Types.VillainousChoiceRewrite as VillainousChoiceRewrite

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.VillainousChoiceR" $ do
  -- CR 701.55c: The Valeyard.
  Spec.it s "MkVillainousChoiceR" $
    Common.assertCodec
      s
      VillainousChoiceR.codec
      (VillainousChoiceR.MkVillainousChoiceR (ControllerRelation.Related PlayerRelation.Opponent) VillainousChoiceRewrite.AdditionalTime)
      " {\"whose\":{\"type\":\"Opponent\"},\"rewrite\":{\"type\":\"AdditionalTime\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s VillainousChoiceR.codec
