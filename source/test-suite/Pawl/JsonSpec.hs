-- Covers Pawl.Type.Decimal, Pawl.Type.Json, Pawl.Json.
module Pawl.JsonSpec where

import qualified Pawl.Type.Decimal as Decimal
import qualified Pawl.Type.Json as Json
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.JsonSpec"
    [ Tasty.testGroup
        "Decimal.mkDecimal"
        [ HU.testCase "strips trailing zeros into the exponent" $
            HU.assertEqual "1200" (Decimal.MkDecimal 12 2) (Decimal.mkDecimal 1200 0),
          HU.testCase "zero mantissa normalizes" $
            HU.assertEqual "0" (Decimal.MkDecimal 0 0) (Decimal.mkDecimal 0 5),
          HU.testCase "keeps a bare integer" $
            HU.assertEqual "123" (Decimal.MkDecimal 123 0) (Decimal.mkDecimal 123 0)
        ],
      Tasty.testGroup
        "Value"
        [ HU.testCase "equality distinguishes constructors" $
            HU.assertBool "null /= true" (Json.Null /= Json.Boolean True)
        ]
    ]
