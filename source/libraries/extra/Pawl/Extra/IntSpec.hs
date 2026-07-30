module Pawl.Extra.IntSpec where

import qualified Pawl.Extra.Int as Int
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Int" $ do
  Spec.describe s "toNatural" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Int.toNatural 0) $ Just 0

    Spec.it s "succeeds with a big positive number" $ do
      Spec.assertEq s (Int.toNatural 1234567890) $ Just 1234567890

    Spec.it s "fails with a negative number" $ do
      Spec.assertEq s (Int.toNatural (-1)) Nothing

  Spec.describe s "toNaturalSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Int.toNaturalSaturating 0) 0

    Spec.it s "works with a big positive number" $ do
      Spec.assertEq s (Int.toNaturalSaturating 1234567890) 1234567890

    Spec.it s "converts a negative number to zero" $ do
      Spec.assertEq s (Int.toNaturalSaturating (-1)) 0
