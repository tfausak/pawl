module Pawl.Json.NullSpec where

import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Null as Null
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Null" $ do
  Spec.describe s "decode" $ do
    Spec.it s "fails with no match" $ do
      Spec.assertEq s (Parsec.parseString Null.decode "invalid") Nothing

    Spec.it s "succeeds with a match" $ do
      Spec.assertEq s (Parsec.parseString Null.decode "null") . Just $ Null.MkNull ()
