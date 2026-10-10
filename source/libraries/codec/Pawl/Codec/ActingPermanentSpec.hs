module Pawl.Codec.ActingPermanentSpec where

import qualified Pawl.Codec.ActingPermanent as ActingPermanent
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ActingPermanent as ActingPermanent
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ActingPermanent" $ do
  Spec.it s "Self" $
    Common.assertCodec
      s
      ActingPermanent.codec
      ActingPermanent.Self
      " {\"type\":\"Self\"} "
  Spec.it s "Matching" $
    Common.assertCodec
      s
      ActingPermanent.codec
      (ActingPermanent.Matching (Filter.ControlledBy PlayerRelation.You))
      " {\"type\":\"Matching\",\"value\":{\"type\":\"ControlledBy\",\"value\":{\"type\":\"You\"}}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s ActingPermanent.codec
