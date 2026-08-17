module Pawl.Codec.SubtypeFamilySpec where

import qualified Pawl.Codec.SubtypeFamily as SubtypeFamily
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SubtypeFamily as SubtypeFamily

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SubtypeFamily" $ do
  Spec.it s "BasicLandType" $
    Common.assertCodec
      s
      SubtypeFamily.codec
      SubtypeFamily.BasicLandType
      " {\"type\":\"BasicLandType\"} "
  Spec.it s "CreatureType" $
    Common.assertCodec
      s
      SubtypeFamily.codec
      SubtypeFamily.CreatureType
      " {\"type\":\"CreatureType\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s SubtypeFamily.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s SubtypeFamily.codec
