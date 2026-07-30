module Pawl.Extra.SemigroupSpec where

import qualified Pawl.Extra.Semigroup as Semigroup
import qualified Pawl.Spec as Spec

spec :: (Applicative m) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Extra.Semigroup" $ do
  Spec.describe s "around" $ do
    Spec.it s "works" $ do
      Spec.assertEq s (Semigroup.around "<" ">" "x") "<x>"
