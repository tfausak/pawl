module Pawl.DecimalSpec where

import qualified Pawl.Decimal as Decimal
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Decimal" $ do
  Spec.describe s "mkDecimal" $ do
    Spec.it s "accepts normalized zero" $ do
      Spec.assertEq s (Decimal.mkDecimal 0 0) $ Decimal.UnsafeDecimal 0 0

    Spec.it s "normalizes zero" $ do
      Spec.assertEq s (Decimal.mkDecimal 0 1) $ Decimal.UnsafeDecimal 0 0

    Spec.it s "accepts one" $ do
      Spec.assertEq s (Decimal.mkDecimal 1 0) $ Decimal.UnsafeDecimal 1 0

    Spec.it s "accepts normalized twenty" $ do
      Spec.assertEq s (Decimal.mkDecimal 2 1) $ Decimal.UnsafeDecimal 2 1

    Spec.it s "normalizes twenty" $ do
      Spec.assertEq s (Decimal.mkDecimal 20 0) $ Decimal.UnsafeDecimal 2 1

    Spec.it s "accepts three hundred" $ do
      Spec.assertEq s (Decimal.mkDecimal 3 2) $ Decimal.UnsafeDecimal 3 2

    Spec.it s "partially normalizes three hundred" $ do
      Spec.assertEq s (Decimal.mkDecimal 30 1) $ Decimal.UnsafeDecimal 3 2

    Spec.it s "fully normalizes three hundred" $ do
      Spec.assertEq s (Decimal.mkDecimal 300 0) $ Decimal.UnsafeDecimal 3 2

    Spec.it s "accepts negative one" $ do
      Spec.assertEq s (Decimal.mkDecimal (-1) 0) $ Decimal.UnsafeDecimal (-1) 0

    Spec.it s "accepts two tenths" $ do
      Spec.assertEq s (Decimal.mkDecimal 2 (-1)) $ Decimal.UnsafeDecimal 2 (-1)

  Spec.describe s "compare" $ do
    Spec.it s "considers zero equal to itself" $ do
      Spec.assertEq s (compare (Decimal.mkDecimal 0 0) $ Decimal.mkDecimal 0 0) EQ

    Spec.it s "considers an unnormalized zero equal to zero" $ do
      Spec.assertEq s (compare (Decimal.UnsafeDecimal 0 5) $ Decimal.UnsafeDecimal 0 0) EQ

    Spec.it s "orders zero below one" $ do
      Spec.assertLt s (Decimal.mkDecimal 0 0) $ Decimal.mkDecimal 1 0

    Spec.it s "orders one above zero" $ do
      Spec.assertGt s (Decimal.mkDecimal 1 0) $ Decimal.mkDecimal 0 0

    Spec.it s "orders negative one below zero" $ do
      Spec.assertLt s (Decimal.mkDecimal (-1) 0) $ Decimal.mkDecimal 0 0

    Spec.it s "orders negative one below one" $ do
      Spec.assertLt s (Decimal.mkDecimal (-1) 0) $ Decimal.mkDecimal 1 0

    Spec.it s "orders one below two" $ do
      Spec.assertLt s (Decimal.mkDecimal 1 0) $ Decimal.mkDecimal 2 0

    -- Comparing mantissas before exponents would get this backwards.
    Spec.it s "orders nine below one thousand" $ do
      Spec.assertLt s (Decimal.mkDecimal 9 0) $ Decimal.mkDecimal 1 3

    Spec.it s "orders negative one thousand below negative nine" $ do
      Spec.assertLt s (Decimal.mkDecimal (-1) 3) $ Decimal.mkDecimal (-9) 0

    Spec.it s "orders two tenths below one" $ do
      Spec.assertLt s (Decimal.mkDecimal 2 (-1)) $ Decimal.mkDecimal 1 0

    Spec.it s "ignores the representation" $ do
      Spec.assertEq s (compare (Decimal.UnsafeDecimal 20 0) $ Decimal.UnsafeDecimal 2 1) EQ

    Spec.it s "orders eleven and a half below twelve" $ do
      Spec.assertLt s (Decimal.mkDecimal 115 (-1)) $ Decimal.mkDecimal 12 0

    Spec.it s "orders negative twelve below negative eleven and a half" $ do
      Spec.assertLt s (Decimal.mkDecimal (-12) 0) $ Decimal.mkDecimal (-115) (-1)

    -- Scaling both mantissas to a common exponent would build a number with a
    -- trillion digits, so this test hangs unless the magnitudes decide it.
    Spec.it s "orders far apart exponents without scaling" $ do
      Spec.assertGt s (Decimal.mkDecimal 1 (10 ^ (12 :: Integer))) $ Decimal.mkDecimal 2 0

  Spec.describe s "adjustedExponent" $ do
    Spec.it s "is zero for a single digit" $ do
      Spec.assertEq s (Decimal.adjustedExponent $ Decimal.UnsafeDecimal 5 0) 0

    Spec.it s "counts the digits before the exponent" $ do
      Spec.assertEq s (Decimal.adjustedExponent $ Decimal.UnsafeDecimal 115 (-1)) 1

  Spec.describe s "digitCount" $ do
    Spec.it s "counts zero as one digit" $ do
      Spec.assertEq s (Decimal.digitCount 0) 1

    Spec.it s "counts nine as one digit" $ do
      Spec.assertEq s (Decimal.digitCount 9) 1

    Spec.it s "counts ten as two digits" $ do
      Spec.assertEq s (Decimal.digitCount 10) 2

    Spec.it s "ignores the sign" $ do
      Spec.assertEq s (Decimal.digitCount (-100)) 3
