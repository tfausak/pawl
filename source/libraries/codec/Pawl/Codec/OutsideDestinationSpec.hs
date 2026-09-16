module Pawl.Codec.OutsideDestinationSpec where

import qualified Pawl.Codec.OutsideDestination as OutsideDestination
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.OutsideDestination as OutsideDestination

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OutsideDestination" $ do
  Spec.it s "Hand" $
    Common.assertCodec
      s
      OutsideDestination.codec
      OutsideDestination.Hand
      " {\"type\":\"Hand\"} "
  Spec.it s "LibraryTop" $
    Common.assertCodec
      s
      OutsideDestination.codec
      OutsideDestination.LibraryTop
      " {\"type\":\"LibraryTop\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s OutsideDestination.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s OutsideDestination.codec
