module Pawl.Codec.TurnScopeSpec where

import qualified Pawl.Codec.TurnScope as TurnScope
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TurnScope as TurnScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TurnScope" $ do
  Spec.it s "EachTurn" $
    Common.assertCodec
      s
      TurnScope.codec
      TurnScope.EachTurn
      " {\"type\":\"EachTurn\"} "
  Spec.it s "ControllersTurn" $
    Common.assertCodec
      s
      TurnScope.codec
      TurnScope.ControllersTurn
      " {\"type\":\"ControllersTurn\"} "
  Spec.it s "OpponentsTurn" $
    Common.assertCodec
      s
      TurnScope.codec
      TurnScope.OpponentsTurn
      " {\"type\":\"OpponentsTurn\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s TurnScope.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s TurnScope.codec
