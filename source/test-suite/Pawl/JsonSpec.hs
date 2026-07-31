-- Covers Pawl.Json. Encoding and decoding of each JSON shape belong to the json
-- sublibrary's own specs; what is left here is the codec-facing surface: the
-- tagged-object convention, key normalization, and the Text/Either adapters
-- over Pawl.Json.Value's Builder/Parsec pair.
module Pawl.JsonSpec where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as J
import qualified Pawl.Json.Value as Value
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

roundTrips :: Value.Value -> HU.Assertion
roundTrips v = HU.assertEqual "round-trip" (Right v) (J.parse (J.render v))

-- Builds an object without Text.pack noise at every key.
obj :: [(String, Value.Value)] -> Value.Value
obj = J.jObject . fmap (Bifunctor.first Text.pack)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.JsonSpec"
    [ Tasty.testGroup
        "tagged"
        [ HU.testCase "a nullary tag omits the value key" $
            HU.assertEqual "nullary" (Text.pack "{\"type\":\"ManaValue\"}") (J.render (J.tagged (Text.pack "ManaValue") Nothing)),
          HU.testCase "a payload goes under the value key, after the type" $
            HU.assertEqual
              "payload"
              (Text.pack "{\"type\":\"Literal\",\"value\":5}")
              (J.render (J.tagged (Text.pack "Literal") (Just (J.jInt 5)))),
          HU.testCase "tag reads back what tagged wrote" $
            HU.assertEqual
              "round-trip"
              (Right (Text.pack "Literal", Just (J.jInt 5)))
              (J.tag (J.tagged (Text.pack "Literal") (Just (J.jInt 5))))
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
              (J.jArray [J.jInt 2, J.jInt 1])
              (J.sortKeys (J.jArray [J.jInt 2, J.jInt 1])),
          HU.testCase "sorts objects inside arrays" $
            HU.assertEqual
              "in array"
              (J.jArray [obj [("a", J.jInt 1), ("b", J.jInt 2)]])
              (J.sortKeys (J.jArray [obj [("b", J.jInt 2), ("a", J.jInt 1)]])),
          HU.testCase "leaves an already-sorted value alone" $
            let v = obj [("a", J.jInt 1), ("b", J.jInt 2)]
             in HU.assertEqual "no-op" v (J.sortKeys v),
          HU.testCase "leaves scalars alone" $
            HU.assertEqual "null" J.jNull (J.sortKeys J.jNull),
          HU.testCase "is idempotent" $
            let v = obj [("b", obj [("d", J.jInt 2), ("c", J.jInt 1)]), ("a", J.jNull)]
             in HU.assertEqual "twice" (J.sortKeys v) (J.sortKeys (J.sortKeys v))
        ],
      Tasty.testGroup
        "parse round-trips render"
        [ HU.testCase "integer" $ roundTrips (J.jInt 42),
          HU.testCase "tagged with value" $
            roundTrips (J.tagged (Text.pack "Literal") (Just (J.jInt 3))),
          HU.testCase "nested array/object" $
            roundTrips
              ( J.jArray
                  [ obj [("k", J.jText (Text.pack "v\"x"))],
                    J.jBool True,
                    J.jNull
                  ]
              )
        ],
      Tasty.testGroup
        "parse"
        [ -- Pawl.Json.parse's whole job on top of Pawl.Json.Value.decode: pin
          -- the end of input, so a card file with trailing garbage is an error
          -- rather than a silently truncated read.
          HU.testCase "trailing input is an error, not a prefix parse" $
            HU.assertBool "left" (either (const True) (const False) (J.parse (Text.pack "1 2"))),
          HU.testCase "surrounding blanks are accepted" $
            HU.assertEqual "blanks" (Right (J.jInt 1)) (J.parse (Text.pack " 1 "))
        ]
    ]
