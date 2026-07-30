module Pawl.Json.NumberSpec where

import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Parsec as Parsec
import qualified Pawl.Json.Number as Number
import qualified Pawl.Spec as Spec

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Json.Number" $ do
  Spec.describe s "decode" $ do
    Spec.it s "succeeds with zero" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "0") . Just . Number.MkNumber $ Decimal.mkDecimal 0 0

    Spec.it s "succeeds with negative zero" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "-0") . Just . Number.MkNumber $ Decimal.mkDecimal 0 0

    Spec.it s "succeeds with a positive integer" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "12") . Just . Number.MkNumber $ Decimal.mkDecimal 12 0

    Spec.it s "succeeds with a negative integer" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "-23") . Just . Number.MkNumber $ Decimal.mkDecimal (-23) 0

    Spec.it s "succeeds with a positive decimal" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "34.56") . Just . Number.MkNumber $ Decimal.mkDecimal 3456 (-2)

    Spec.it s "succeeds with a negative decimal" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "-45.67") . Just . Number.MkNumber $ Decimal.mkDecimal (-4567) (-2)

    Spec.it s "succeeds with a positive exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "56e78") . Just . Number.MkNumber $ Decimal.mkDecimal 56 78

    Spec.it s "succeeds with a negative exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "67e-89") . Just . Number.MkNumber $ Decimal.mkDecimal 67 (-89)

    Spec.it s "succeeds with an explicitly positive exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "56e+78") . Just . Number.MkNumber $ Decimal.mkDecimal 56 78

    Spec.it s "succeeds with an uppercase exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "56E78") . Just . Number.MkNumber $ Decimal.mkDecimal 56 78

    Spec.it s "succeeds with leading zero fraction" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "0.5") . Just . Number.MkNumber $ Decimal.mkDecimal 5 (-1)

    Spec.it s "succeeds with unnecessary fraction" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5.0") . Just . Number.MkNumber $ Decimal.mkDecimal 5 0

    Spec.it s "succeeds with zero exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5e0") . Just . Number.MkNumber $ Decimal.mkDecimal 5 0

    Spec.it s "succeeds with positive zero exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5e+0") . Just . Number.MkNumber $ Decimal.mkDecimal 5 0

    Spec.it s "succeeds with negative zero exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5e-0") . Just . Number.MkNumber $ Decimal.mkDecimal 5 0

    Spec.it s "succeeds with multiple zeros in fraction" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5.00") . Just . Number.MkNumber $ Decimal.mkDecimal 5 0

    Spec.it s "fails with only minus sign" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "-") Nothing

    Spec.it s "fails with explicit positive integer" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "+5") Nothing

    Spec.it s "fails with explicit positive fraction" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "0.+5") Nothing

    Spec.it s "fails with leading zero" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "05") Nothing

    Spec.it s "fails with trailing decimal point" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5.") Nothing

    Spec.it s "fails with leading decimal point" $ do
      Spec.assertEq s (Parsec.parseString Number.decode ".5") Nothing

    Spec.it s "fails with trailing exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "5e") Nothing

    Spec.it s "fails with leading exponent" $ do
      Spec.assertEq s (Parsec.parseString Number.decode "e5") Nothing

  Spec.describe s "encode" $ do
    Spec.it s "encodes zero" $ do
      Spec.assertEq s (Builder.toString . Number.encode . Number.MkNumber $ Decimal.mkDecimal 0 0) "0"

    Spec.it s "encodes positive integer" $ do
      Spec.assertEq s (Builder.toString . Number.encode . Number.MkNumber $ Decimal.mkDecimal 123 0) "123"

    Spec.it s "encodes negative integer" $ do
      Spec.assertEq s (Builder.toString . Number.encode . Number.MkNumber $ Decimal.mkDecimal (-234) 0) "-234"

    Spec.it s "encodes with positive exponent" $ do
      Spec.assertEq s (Builder.toString . Number.encode . Number.MkNumber $ Decimal.mkDecimal 345 2) "345e2"

    Spec.it s "encodes with negative exponent" $ do
      Spec.assertEq s (Builder.toString . Number.encode . Number.MkNumber $ Decimal.mkDecimal 456 (-2)) "456e-2"
