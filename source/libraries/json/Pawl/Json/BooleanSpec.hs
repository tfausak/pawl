module Pawl.Json.BooleanSpec where

import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Boolean" $ do
  Spec.describe s "decode" $ do
    Spec.it s "fails with no match" $ do
      Spec.assertEq s (Parsec.parseString Boolean.decode "invalid") Nothing

    Spec.it s "succeeds with false" $ do
      Spec.assertEq s (Parsec.parseString Boolean.decode "false") . Just $ Boolean.MkBoolean False

    Spec.it s "succeeds with true" $ do
      Spec.assertEq s (Parsec.parseString Boolean.decode "true") . Just $ Boolean.MkBoolean True

  Spec.describe s "encode" $ do
    Spec.it s "encodes false" $ do
      Spec.assertEq s (Builder.toString . Boolean.encode $ Boolean.MkBoolean False) "false"

    Spec.it s "encodes true" $ do
      Spec.assertEq s (Builder.toString . Boolean.encode $ Boolean.MkBoolean True) "true"
