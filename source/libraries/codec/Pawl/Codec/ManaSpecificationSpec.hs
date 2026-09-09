module Pawl.Codec.ManaSpecificationSpec where

import qualified Pawl.Codec.ManaSpecification as ManaSpecification
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ManaSpecification as ManaSpecification

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ManaSpecification" $ do
  Spec.it s "AnyMana" $
    Common.assertCodec
      s
      ManaSpecification.codec
      ManaSpecification.AnyMana
      " {\"type\":\"AnyMana\"} "
  Spec.it s "ChosenColor" $
    Common.assertCodec
      s
      ManaSpecification.codec
      ManaSpecification.ChosenColor
      " {\"type\":\"ChosenColor\"} "
  -- Exhaustive where the literals above are representative, Pawl.Codec.PlayerRelationSpec's
  -- reason: Arm.enum derives the arm list from the type.
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s ManaSpecification.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s ManaSpecification.codec
