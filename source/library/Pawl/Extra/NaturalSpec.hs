module Pawl.Extra.NaturalSpec where

import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Natural" $ do
  Spec.describe s "length" $ do
    Spec.it s "works with no elements" $ do
      Spec.assertEq s (Natural.length "") 0

    Spec.it s "works with one element" $ do
      Spec.assertEq s (Natural.length "a") 1

    Spec.it s "works with more than one element" $ do
      Spec.assertEq s (Natural.length "bc") 2

  Spec.describe s "toInt" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Natural.toInt 0) $ Just 0

    Spec.it s "succeeds with the max bound" $ do
      Spec.assertEq s (Natural.toInt 9223372036854775807) $ Just 9223372036854775807

    Spec.it s "fails beyond the max bound" $ do
      Spec.assertEq s (Natural.toInt 9223372036854775808) Nothing

  Spec.describe s "toIntSaturating" $ do
    Spec.it s "works with zero" $ do
      Spec.assertEq s (Natural.toIntSaturating 0) 0

    Spec.it s "works with the max bound" $ do
      Spec.assertEq s (Natural.toIntSaturating 9223372036854775807) 9223372036854775807

    Spec.it s "clamps to the max bound" $ do
      Spec.assertEq s (Natural.toIntSaturating 9223372036854775808) 9223372036854775807

  Spec.describe s "minusSaturating" $ do
    Spec.it s "subtracts when the result is not negative" $ do
      Spec.assertEq s (Natural.minusSaturating 3 2) 1

    Spec.it s "works when the result is zero" $ do
      Spec.assertEq s (Natural.minusSaturating 2 2) 0

    Spec.it s "clamps to zero" $ do
      Spec.assertEq s (Natural.minusSaturating 2 3) 0
