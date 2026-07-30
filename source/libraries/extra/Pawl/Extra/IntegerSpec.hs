module Pawl.Extra.IntegerSpec where

import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Integer" $ do
  Spec.describe s "toInt" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Integer.toInt 0) $ Just 0

    Spec.it s "succeeds with the max bound" $ do
      Spec.assertEq s (Integer.toInt 9223372036854775807) $ Just 9223372036854775807

    Spec.it s "fails beyond the max bound" $ do
      Spec.assertEq s (Integer.toInt 9223372036854775808) Nothing

    Spec.it s "succeeds with the min bound" $ do
      Spec.assertEq s (Integer.toInt (-9223372036854775808)) $ Just (-9223372036854775808)

    Spec.it s "fails beyond the min bound" $ do
      Spec.assertEq s (Integer.toInt (-9223372036854775809)) Nothing

  Spec.describe s "toIntSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Integer.toIntSaturating 0) 0

    Spec.it s "works with the max bound" $ do
      Spec.assertEq s (Integer.toIntSaturating 9223372036854775807) 9223372036854775807

    Spec.it s "clamps to the max bound" $ do
      Spec.assertEq s (Integer.toIntSaturating 9223372036854775808) 9223372036854775807

    Spec.it s "works with the min bound" $ do
      Spec.assertEq s (Integer.toIntSaturating (-9223372036854775808)) (-9223372036854775808)

    Spec.it s "clamps to the min bound" $ do
      Spec.assertEq s (Integer.toIntSaturating (-9223372036854775809)) (-9223372036854775808)

  Spec.describe s "toNatural" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Integer.toNatural 0) $ Just 0

    Spec.it s "succeeds with a big positive number" $ do
      Spec.assertEq s (Integer.toNatural 1234567890) $ Just 1234567890

    Spec.it s "fails with a negative number" $ do
      Spec.assertEq s (Integer.toNatural (-1)) Nothing

  Spec.describe s "toNaturalSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Integer.toNaturalSaturating 0) 0

    Spec.it s "works with a big positive number" $ do
      Spec.assertEq s (Integer.toNaturalSaturating 1234567890) 1234567890

    Spec.it s "converts a negative number to zero" $ do
      Spec.assertEq s (Integer.toNaturalSaturating (-1)) 0
