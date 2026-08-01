module Pawl.Codec.CommonSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Common" $ do
  Spec.describe s "parse" $ do
    Spec.it s "rejects trailing input" $
      Spec.assertBool s (Either.isLeft . Common.parse $ Text.pack "\"a\" x") "expected a parse failure"
    Spec.it s "round trips through render" $
      Spec.assertEq s (Common.parse (Common.render (Common.array [Common.integer 1]))) (Right (Common.array [Common.integer 1]))

  Spec.describe s "tagged" $ do
    Spec.it s "omits an absent value" $
      Spec.assertEq s (Common.render (Common.tagged "ManaValue" Nothing)) (Text.pack "{\"type\":\"ManaValue\"}")
    Spec.it s "includes a present value" $
      Spec.assertEq s (Common.render (Common.tagged "Literal" (Just (Common.integer 5)))) (Text.pack "{\"type\":\"Literal\",\"value\":5}")

  Spec.describe s "asTagged" . Spec.it s "returns a String tag" $
    Spec.assertEq s (Common.asTagged (Common.nullary "X")) (Right ("X", Nothing))

  Spec.describe s "sortKeys" . Spec.it s "orders object keys" $
    Spec.assertEq
      s
      (Common.sortKeys (Common.object [Common.pair "b" (Common.integer 1), Common.pair "a" (Common.integer 2)]))
      (Common.object [Common.pair "a" (Common.integer 2), Common.pair "b" (Common.integer 1)])

  Spec.describe s "assertToJson" . Spec.it s "ignores object key order" $
    Common.assertToJson s id (Common.object [Common.pair "b" (Common.integer 1), Common.pair "a" (Common.integer 2)]) "{\"a\":2,\"b\":1}"
