module Pawl.Extra.OrdSpec where

import qualified Pawl.Extra.Ord as Ord
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Ord" $ do
  Spec.describe s "between" $ do
    Spec.it s "is false below the lower bound" $ do
      Spec.assertEq s (Ord.between '4' '6' '3') False

    Spec.it s "is true at the lower bound" $ do
      Spec.assertEq s (Ord.between '4' '6' '4') True

    Spec.it s "is true between the bounds" $ do
      Spec.assertEq s (Ord.between '4' '6' '5') True

    Spec.it s "is true at the upper bound" $ do
      Spec.assertEq s (Ord.between '4' '6' '6') True

    Spec.it s "is false above the upper bound" $ do
      Spec.assertEq s (Ord.between '4' '6' '7') False
