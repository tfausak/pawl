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
  -- CR 113.3b's limb, which CR 602.2b routes through the same targeting step.
  Spec.it s "ActivatedAbility" $
    Common.assertCodec
      s
      StackObjectKind.codec
      StackObjectKind.ActivatedAbility
      " {\"type\":\"ActivatedAbility\"} "
  -- CR 113.3c's, which CR 603.3d routes through it as well. Professor Hojo's
  -- "an activated ability" is the printing that tells the two apart.
  Spec.it s "TriggeredAbility" $
    Common.assertCodec
      s
      StackObjectKind.codec
      StackObjectKind.TriggeredAbility
      " {\"type\":\"TriggeredAbility\"} "
  -- Exhaustive where the literals above are representative: Arm.enum derives
  -- the arm list from the type, so this is what would catch a constructor the
  -- derivation missed or two that encode alike.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s StackObjectKind.codec
  Spec.it s "has a schema" $
    Common.assertHasSchema s StackObjectKind.codec
