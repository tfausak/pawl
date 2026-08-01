module Pawl.Codec.PlayerRelationSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.PlayerRelation as PlayerRelation
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerRelation as PlayerRelation

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerRelation" $ do
  Spec.it s "You" $
    Common.assertJsonCodec s PlayerRelation.toJson PlayerRelation.fromJson PlayerRelation.You "{\"type\":\"You\"}"
  Spec.it s "Opponent" $
    Common.assertJsonCodec s PlayerRelation.toJson PlayerRelation.fromJson PlayerRelation.Opponent "{\"type\":\"Opponent\"}"
