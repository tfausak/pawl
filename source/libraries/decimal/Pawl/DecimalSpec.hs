module Pawl.DecimalSpec where

import qualified Pawl.Decimal as Decimal
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Decimal" $ do
  Spec.describe s "mkDecimal" $ do
    Spec.it s "accepts normalized zero" $ do
      Spec.assertEq s (Decimal.mkDecimal 0 0) (Decimal.UnsafeDecimal 0 0)

    Spec.it s "normalizes zero" $ do
      Spec.assertEq s (Decimal.mkDecimal 0 1) (Decimal.UnsafeDecimal 0 0)

    Spec.it s "accepts one" $ do
      Spec.assertEq s (Decimal.mkDecimal 1 0) (Decimal.UnsafeDecimal 1 0)

    Spec.it s "accepts normalized twenty" $ do
      Spec.assertEq s (Decimal.mkDecimal 2 1) (Decimal.UnsafeDecimal 2 1)

    Spec.it s "normalizes twenty" $ do
      Spec.assertEq s (Decimal.mkDecimal 20 0) (Decimal.UnsafeDecimal 2 1)

    Spec.it s "accepts three hundred" $ do
      Spec.assertEq s (Decimal.mkDecimal 3 2) (Decimal.UnsafeDecimal 3 2)

    Spec.it s "partially normalizes three hundred" $ do
      Spec.assertEq s (Decimal.mkDecimal 30 1) (Decimal.UnsafeDecimal 3 2)

    Spec.it s "fully normalizes three hundred" $ do
      Spec.assertEq s (Decimal.mkDecimal 300 0) (Decimal.UnsafeDecimal 3 2)

    Spec.it s "accepts negative one" $ do
      Spec.assertEq s (Decimal.mkDecimal (-1) 0) (Decimal.UnsafeDecimal (-1) 0)

    Spec.it s "accepts two tenths" $ do
      Spec.assertEq s (Decimal.mkDecimal 2 (-1)) (Decimal.UnsafeDecimal 2 (-1))
