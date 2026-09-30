module Pawl.Codec.OptionalDecisionSpec where

import qualified Pawl.Codec.OptionalDecision as OptionalDecision
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.OptionalDecision as OptionalDecision.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.OptionalDecision" $ do
  Spec.it s "Exercises" $
    Common.assertCodec s OptionalDecision.codec OptionalDecision.Type.Exercises " {\"type\":\"Exercises\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s OptionalDecision.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s OptionalDecision.codec
