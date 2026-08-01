-- Covers Pawl.Codec.Json. Encoding and decoding of each JSON shape belong to the
-- json sublibrary's own specs; what is left here is the codec-facing surface: the
-- tagged-object convention, key normalization, and the Text/Either adapters
-- over Pawl.Json.Value's Builder/Parsec pair.
module Pawl.JsonSpec where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as J
import qualified Pawl.Json.Value as Value
import qualified Pawl.Spec as Spec

roundTrips :: (Applicative m) => Spec.Spec m n -> Value.Value -> m ()
roundTrips s v = Spec.assertEq s (J.parse (J.render v)) $ Right v

-- Builds an object without Text.pack noise at every key.
obj :: [(String, Value.Value)] -> Value.Value
obj = J.jObject . fmap (Bifunctor.first Text.pack)

spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Json" $ do
  Spec.describe s "tagged" $ do
    Spec.it s "a nullary tag omits the value key" $ do
      Spec.assertEq s (J.render (J.tagged (Text.pack "ManaValue") Nothing)) . Text.pack $ "{\"type\":\"ManaValue\"}"

    Spec.it s "a payload goes under the value key, after the type" $ do
      Spec.assertEq s (J.render (J.tagged (Text.pack "Literal") (Just (J.jInt 5)))) . Text.pack $ "{\"type\":\"Literal\",\"value\":5}"

    Spec.it s "tag reads back what tagged wrote" $ do
      Spec.assertEq s (J.tag (J.tagged (Text.pack "Literal") (Just (J.jInt 5)))) . Right $ (Text.pack "Literal", Just (J.jInt 5))

  Spec.describe s "sortKeys" $ do
    Spec.it s "sorts an object's keys" $ do
      Spec.assertEq s (J.sortKeys (obj [("b", J.jInt 1), ("a", J.jInt 2)])) $ obj [("a", J.jInt 2), ("b", J.jInt 1)]

    Spec.it s "sorts nested objects" $ do
      Spec.assertEq s (J.sortKeys (obj [("a", obj [("d", J.jInt 2), ("c", J.jInt 1)])])) $ obj [("a", obj [("c", J.jInt 1), ("d", J.jInt 2)])]

    Spec.it s "preserves array order" $ do
      Spec.assertEq s (J.sortKeys (J.jArray [J.jInt 2, J.jInt 1])) $ J.jArray [J.jInt 2, J.jInt 1]

    Spec.it s "sorts objects inside arrays" $ do
      Spec.assertEq s (J.sortKeys (J.jArray [obj [("b", J.jInt 2), ("a", J.jInt 1)]])) $ J.jArray [obj [("a", J.jInt 1), ("b", J.jInt 2)]]

    Spec.it s "leaves an already-sorted value alone" $ do
      let v = obj [("a", J.jInt 1), ("b", J.jInt 2)]
      Spec.assertEq s (J.sortKeys v) v

    Spec.it s "leaves scalars alone" $ do
      Spec.assertEq s (J.sortKeys J.jNull) J.jNull

    Spec.it s "is idempotent" $ do
      let v = obj [("b", obj [("d", J.jInt 2), ("c", J.jInt 1)]), ("a", J.jNull)]
      Spec.assertEq s (J.sortKeys (J.sortKeys v)) $ J.sortKeys v

  Spec.describe s "parse round-trips render" $ do
    Spec.it s "integer" $ do
      roundTrips s $ J.jInt 42

    Spec.it s "tagged with value" $ do
      roundTrips s . J.tagged (Text.pack "Literal") . Just $ J.jInt 3

    Spec.it s "nested array/object" $ do
      roundTrips s $
        J.jArray
          [ obj [("k", J.jText (Text.pack "v\"x"))],
            J.jBool True,
            J.jNull
          ]

  Spec.describe s "parse" $ do
    -- Pawl.Json.parse's whole job on top of Pawl.Json.Value.decode: pin
    -- the end of input, so a card file with trailing garbage is an error
    -- rather than a silently truncated read.
    Spec.it s "trailing input is an error, not a prefix parse" $ do
      Spec.assertBool s (either (const True) (const False) (J.parse (Text.pack "1 2"))) "left"

    Spec.it s "surrounding blanks are accepted" $ do
      Spec.assertEq s (J.parse (Text.pack " 1 ")) . Right $ J.jInt 1
