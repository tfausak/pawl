module Pawl.Extra.MonoidSpec where

import qualified Pawl.Extra.Monoid as Monoid
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Monoid" $ do
  Spec.describe s "sepBy" $ do
    Spec.it s "works with an empty list" $ do
      Spec.assertEq s (Monoid.sepBy "," []) ""

    Spec.it s "works with one element" $ do
      Spec.assertEq s (Monoid.sepBy "," ["a"]) "a"

    Spec.it s "works with two elements" $ do
      Spec.assertEq s (Monoid.sepBy "," ["a", "b"]) "a,b"
