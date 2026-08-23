module Pawl.Codec.PlayerStaticAbilitySpec where

import qualified Pawl.Codec.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.PlayerStaticAbility as PlayerStaticAbility
import qualified Pawl.Types.Quantity as Quantity

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PlayerStaticAbility" $ do
  Spec.it s "MkPlayerStaticAbility" $
    Common.assertCodec
      s
      PlayerStaticAbility.codec
      (PlayerStaticAbility.MkPlayerStaticAbility PlayerScope.EachPlayer Nothing (PlayerEffect.CantCastMoreThan 1))
      " {\"scope\":{\"type\":\"EachPlayer\"},\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":1}} "
  -- The CR 604.2 clause round-trips as its own key, and the case above proves the
  -- absent key still decodes -- so a decoder that dropped the field would keep
  -- that one green and fail here.
  Spec.it s "MkPlayerStaticAbility with a condition" $
    Common.assertCodec
      s
      PlayerStaticAbility.codec
      ( PlayerStaticAbility.MkPlayerStaticAbility
          PlayerScope.EachPlayer
          (Just (Condition.Compares (Compares.MkCompares (Quantity.IsActivePlayer (PlayerRef.Relative PlayerRelation.You)) Comparison.Exactly (Quantity.Literal 1))))
          (PlayerEffect.CantCastMoreThan 1)
      )
      " {\"scope\":{\"type\":\"EachPlayer\"},\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"IsActivePlayer\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}},\"comparison\":{\"type\":\"Exactly\"},\"threshold\":{\"type\":\"Literal\",\"value\":1}}},\"effect\":{\"type\":\"CantCastMoreThan\",\"value\":1}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s PlayerStaticAbility.codec
