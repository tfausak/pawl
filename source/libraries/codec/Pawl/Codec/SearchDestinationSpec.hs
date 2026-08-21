module Pawl.Codec.SearchDestinationSpec where

import qualified Pawl.Codec.SearchDestination as SearchDestination
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SearchDestination as SearchDestination

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SearchDestination" $ do
  Spec.it s "BattlefieldTapped" $
    Common.assertCodec
      s
      SearchDestination.codec
      SearchDestination.BattlefieldTapped
      " {\"type\":\"BattlefieldTapped\"} "
  Spec.it s "RevealThenHand" $
    Common.assertCodec
      s
      SearchDestination.codec
      SearchDestination.RevealThenHand
      " {\"type\":\"RevealThenHand\"} "
  Spec.it s "Exile" $
    Common.assertCodec
      s
      SearchDestination.codec
      SearchDestination.Exile
      " {\"type\":\"Exile\"} "
  Spec.it s "BattlefieldAttachedToSource" $
    Common.assertCodec
      s
      SearchDestination.codec
      SearchDestination.BattlefieldAttachedToSource
      " {\"type\":\"BattlefieldAttachedToSource\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s SearchDestination.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s SearchDestination.codec
