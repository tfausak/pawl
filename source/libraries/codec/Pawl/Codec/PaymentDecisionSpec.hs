module Pawl.Codec.PaymentDecisionSpec where

import qualified Pawl.Codec.PaymentDecision as PaymentDecision
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PaymentDecision as PaymentDecision.Type

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.PaymentDecision" $ do
  Spec.it s "Pays" $
    Common.assertCodec s PaymentDecision.codec PaymentDecision.Type.Pays " {\"type\":\"Pays\"} "
  Spec.it s "round trips every constructor" $ Common.assertEnumCodec s PaymentDecision.codec
  Spec.it s "has a schema" $ Common.assertHasSchema s PaymentDecision.codec
