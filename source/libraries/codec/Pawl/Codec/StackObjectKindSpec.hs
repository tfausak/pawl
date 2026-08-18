module Pawl.Codec.StackObjectKindSpec where

import qualified Pawl.Codec.StackObjectKind as StackObjectKind
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.StackObjectKind as StackObjectKind

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.StackObjectKind" $ do
  -- CR 112.1's limb, Dormant Gomazoa's.
  Spec.it s "Spell" $
    Common.assertCodec
      s
      StackObjectKind.codec
      StackObjectKind.Spell
      " {\"type\":\"Spell\"} "
  -- CR 113.3's limb, which CR 602.2b and CR 603.3d route through the same
  -- targeting step.
  Spec.it s "Ability" $
    Common.assertCodec
      s
      StackObjectKind.codec
      StackObjectKind.Ability
      " {\"type\":\"Ability\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s StackObjectKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s StackObjectKind.codec
