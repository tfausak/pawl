-- Covers Pawl.Type.Decimal, Pawl.Type.Json, Pawl.Json.
module Pawl.JsonSpec where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Pawl.Json as J
import qualified Pawl.Type.Decimal as Decimal
import qualified Pawl.Type.Json as Json
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

roundTrips :: Json.Value -> HU.Assertion
roundTrips v = HU.assertEqual "round-trip" (Right v) (J.parse (J.render v))

-- Builds an object without Text.pack noise at every key.
obj :: [(String, Json.Value)] -> Json.Value
obj ps = Json.Object (map (Bifunctor.first Text.pack) ps)

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
        ],
      Tasty.testGroup
        "render"
        [ HU.testCase "renders an integer" $
            HU.assertEqual "5" (Text.pack "5") (J.render (J.jInt 5)),
          HU.testCase "renders a tagged nullary" $
            HU.assertEqual "tag" (Text.pack "{\"type\":\"ManaValue\"}") (J.render (J.tagged (Text.pack "ManaValue") Nothing)),
          HU.testCase "escapes strings" $
            HU.assertEqual "quote" (Text.pack "\"a\\\"b\"") (J.render (J.jText (Text.pack "a\"b")))
        ],
      Tasty.testGroup
        "sortKeys"
        [ HU.testCase "sorts an object's keys" $
            HU.assertEqual
              "b before a"
              (obj [("a", J.jInt 2), ("b", J.jInt 1)])
              (J.sortKeys (obj [("b", J.jInt 1), ("a", J.jInt 2)])),
          HU.testCase "sorts nested objects" $
            HU.assertEqual
              "nested"
              (obj [("a", obj [("c", J.jInt 1), ("d", J.jInt 2)])])
              (J.sortKeys (obj [("a", obj [("d", J.jInt 2), ("c", J.jInt 1)])])),
          HU.testCase "preserves array order" $
            HU.assertEqual
              "descending stays descending"
              (Json.Array [J.jInt 2, J.jInt 1])
              (J.sortKeys (Json.Array [J.jInt 2, J.jInt 1])),
          HU.testCase "sorts objects inside arrays" $
            HU.assertEqual
              "in array"
              (Json.Array [obj [("a", J.jInt 1), ("b", J.jInt 2)]])
              (J.sortKeys (Json.Array [obj [("b", J.jInt 2), ("a", J.jInt 1)]])),
          HU.testCase "leaves an already-sorted value alone" $
            let v = obj [("a", J.jInt 1), ("b", J.jInt 2)]
             in HU.assertEqual "no-op" v (J.sortKeys v),
          HU.testCase "leaves scalars alone" $
            HU.assertEqual "null" Json.Null (J.sortKeys Json.Null),
          HU.testCase "is idempotent" $
            let v = obj [("b", obj [("d", J.jInt 2), ("c", J.jInt 1)]), ("a", Json.Null)]
             in HU.assertEqual "twice" (J.sortKeys v) (J.sortKeys (J.sortKeys v))
        ],
      Tasty.testGroup
        "parse round-trips render"
        [ HU.testCase "integer" $ roundTrips (J.jInt 42),
          HU.testCase "tagged with value" $
            roundTrips (J.tagged (Text.pack "Literal") (Just (J.jInt 3))),
          HU.testCase "nested array/object" $
            roundTrips
              ( Json.Array
                  [ Json.Object [(Text.pack "k", J.jText (Text.pack "v\"x"))],
                    Json.Boolean True,
                    Json.Null
                  ]
              )
        ]
    ]
