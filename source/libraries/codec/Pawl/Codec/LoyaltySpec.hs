module Pawl.Codec.LoyaltySpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Loyalty as Loyalty
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Loyalty as Loyalty

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Loyalty" $ do
  Spec.it s "Literal" $
    Common.assertCodec
      s
      Loyalty.codec
      (Loyalty.Literal 3)
      " {\"type\":\"Literal\",\"value\":3} "

  Spec.it s "Variable" $
    Common.assertCodec
      s
      Loyalty.codec
      Loyalty.Variable
      " {\"type\":\"Variable\"} "

  Spec.it s "has a schema" $
    Common.assertHasSchema s Loyalty.codec

  -- The Literal payload is Natural, so a negative number is a decode failure
  -- rather than a wrapped negative going through a partial fromInteger.
  Spec.it s "rejects a negative number" $
    Spec.assertBool
      s
      (Either.isLeft (Common.parse (Text.pack " {\"type\":\"Literal\",\"value\":-1} ") >>= Codec.decode Loyalty.codec))
      "expected a decode failure"
