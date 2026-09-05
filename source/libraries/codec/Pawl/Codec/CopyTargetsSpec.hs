module Pawl.Codec.CopyTargetsSpec where

import qualified Pawl.Codec.CopyTargets as CopyTargets
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CopyTargets as CopyTargets
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CopyTargets" $ do
  Spec.it s "Copied" $
    Common.assertCodec s CopyTargets.codec CopyTargets.Copied " {\"type\":\"Copied\"} "
  Spec.it s "ChosenByController" $
    Common.assertCodec s CopyTargets.codec CopyTargets.ChosenByController " {\"type\":\"ChosenByController\"} "
  Spec.it s "ForEach" $
    Common.assertCodec
      s
      CopyTargets.codec
      (CopyTargets.ForEach (ObjectRef.EachMatching (Filter.ControlledBy PlayerRelation.You)))
      " {\"type\":\"ForEach\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}}} "
  Spec.it s "Stated" $
    Common.assertCodec
      s
      CopyTargets.codec
      (CopyTargets.Stated (ObjectRef.EachMatching Filter.IsSource))
      " {\"type\":\"Stated\",\"value\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"IsSource\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CopyTargets.codec
