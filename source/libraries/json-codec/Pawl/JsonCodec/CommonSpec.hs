module Pawl.JsonCodec.CommonSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.JsonCodec.Common" $ do
  Spec.describe s "parse" $ do
    Spec.it s "rejects trailing input" $
      Spec.assertBool s (Either.isLeft . Common.parse $ Text.pack "\"a\" x") "expected a parse failure"
    Spec.it s "round trips through render" $
      Spec.assertEq s (Common.parse (Common.render (Value.array [Value.integer 1]))) (Right (Value.array [Value.integer 1]))
    -- The array-of-integer case above exercises no string escaping, boolean,
    -- null, or nested object, so this is not a duplicate of it.
    Spec.it s "round trips a nested, heterogeneous value" $
      let v =
            Value.array
              [ Value.object [Pair.fromString "k" (Value.text (Text.pack "v\"x"))],
                Value.boolean True,
                Value.null
              ]
       in Spec.assertEq s (Common.parse (Common.render v)) (Right v)
    -- Parse must accept blanks on both sides of the document, not merely reject
    -- trailing garbage.
    Spec.it s "accepts surrounding blanks" $
      Spec.assertEq s (Common.parse (Text.pack " 1 ")) (Right (Value.integer 1))

  Spec.describe s "tagged" $ do
    Spec.it s "omits an absent value" $
      Spec.assertEq s (Common.render (Common.tagged "ManaValue" Nothing)) (Text.pack "{\"type\":\"ManaValue\"}")
    Spec.it s "includes a present value" $
      Spec.assertEq s (Common.render (Common.tagged "Literal" (Just (Value.integer 5)))) (Text.pack "{\"type\":\"Literal\",\"value\":5}")

  Spec.describe s "asTagged" $ do
    Spec.it s "returns a String tag" $
      Spec.assertEq s (Common.asTagged (Common.nullary "X")) (Right ("X", Nothing))
    -- The payload-bearing case, which the nullary tag above does not exercise.
    Spec.it s "reads back a tagged value's payload" $
      Spec.assertEq s (Common.asTagged (Common.tagged "Literal" (Just (Value.integer 5)))) (Right ("Literal", Just (Value.integer 5)))

  Spec.describe s "sortKeys" $ do
    Spec.it s "orders object keys" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.object [Pair.fromString "b" (Value.integer 1), Pair.fromString "a" (Value.integer 2)]))
        (Value.object [Pair.fromString "a" (Value.integer 2), Pair.fromString "b" (Value.integer 1)])
    -- sortKeys is load-bearing for every assertToJson in the codec's specs, so
    -- its own coverage stays thorough.
    Spec.it s "sorts nested objects" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.object [Pair.fromString "a" (Value.object [Pair.fromString "d" (Value.integer 2), Pair.fromString "c" (Value.integer 1)])]))
        (Value.object [Pair.fromString "a" (Value.object [Pair.fromString "c" (Value.integer 1), Pair.fromString "d" (Value.integer 2)])])
    Spec.it s "preserves array order" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.array [Value.integer 2, Value.integer 1]))
        (Value.array [Value.integer 2, Value.integer 1])
    Spec.it s "sorts objects inside arrays" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.array [Value.object [Pair.fromString "b" (Value.integer 2), Pair.fromString "a" (Value.integer 1)]]))
        (Value.array [Value.object [Pair.fromString "a" (Value.integer 1), Pair.fromString "b" (Value.integer 2)]])
    Spec.it s "leaves an already-sorted value alone" $
      let v = Value.object [Pair.fromString "a" (Value.integer 1), Pair.fromString "b" (Value.integer 2)]
       in Spec.assertEq s (Common.sortKeys v) v
    Spec.it s "leaves scalars alone" $
      Spec.assertEq s (Common.sortKeys Value.null) Value.null
    Spec.it s "is idempotent" $
      let v =
            Value.object
              [ Pair.fromString "b" (Value.object [Pair.fromString "d" (Value.integer 2), Pair.fromString "c" (Value.integer 1)]),
                Pair.fromString "a" Value.null
              ]
       in Spec.assertEq s (Common.sortKeys (Common.sortKeys v)) (Common.sortKeys v)

  Spec.describe s "assertToJson"
    . Spec.it s "ignores object key order"
    $ Common.assertToJson s id (Value.object [Pair.fromString "b" (Value.integer 1), Pair.fromString "a" (Value.integer 2)]) "{\"a\":2,\"b\":1}"

  Spec.describe s "optionalPair" $ do
    Spec.it s "omits a field equal to its default" $
      Spec.assertEq s (Common.optionalPair "k" (0 :: Integer) Value.integer 0) []
    Spec.it s "writes a field differing from its default" $
      Spec.assertEq s (Common.optionalPair "k" (0 :: Integer) Value.integer 1) [Pair.fromString "k" (Value.integer 1)]
    -- The default is not required to be the type's zero.
    Spec.it s "omits a non-zero default" $
      Spec.assertEq s (Common.optionalPair "k" (7 :: Integer) Value.integer 7) []

  Spec.describe s "requiredPair"
    . Spec.it s "always writes the field"
    $ Spec.assertEq s (Common.requiredPair "k" Value.integer (0 :: Integer)) [Pair.fromString "k" (Value.integer 0)]

  Spec.describe s "defaultedField" $ do
    Spec.it s "supplies the default for an absent key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger []) (Right 0)
    Spec.it s "decodes a present key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger [Pair.fromString "k" (Value.integer 1)]) (Right 1)
    -- A present null goes to the decoder rather than short-circuiting to the
    -- default, which is what lets decodeMaybe keep accepting an explicit null.
    Spec.it s "hands a present null to the decoder" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (Just (1 :: Integer)) (Common.decodeMaybe Common.asInteger) [Pair.fromString "k" Value.null])
        (Right Nothing)
    -- The round trip the two halves have to agree on: 'optionalPair' elides a
    -- field equal to its default, so 'defaultedField' sees an absent key and
    -- supplies that same default back.
    Spec.it s "supplies the default when optionalPair elides the field" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (7 :: Integer) Common.asInteger (Common.optionalPair "k" 7 Value.integer 7))
        (Right 7)

  Spec.describe s "defaultedField accepts the verbose form" $ do
    -- The key is PRESENT, so 'defaultedField' hands the value straight to the
    -- decoder; the result equals the default only because 'decodeMaybe' reads
    -- an explicit null as Nothing on its own.
    Spec.it s "an explicit null decodes to Nothing via decodeMaybe, not via the default" $
      Spec.assertEq
        s
        (Common.defaultedField "k" Nothing (Common.decodeMaybe Common.asInteger) [Pair.fromString "k" Value.null])
        (Right (Nothing :: Maybe Integer))
    -- Same shape: 'decodeList' on an explicit empty array, not the default.
    Spec.it s "an explicit empty array decodes to [] via decodeList, not via the default" $
      Spec.assertEq
        s
        (Common.defaultedField "k" [] (Common.decodeList Common.asInteger) [Pair.fromString "k" (Value.array [])])
        (Right ([] :: [Integer]))
    -- An explicit null on a NON-Maybe defaulted field is an error, not the
    -- default: `defaultedField` reads absence directly, so a file that says
    -- `"keywords": null` is malformed rather than empty.
    Spec.it s "an explicit null on a non-Maybe defaulted field is an error" $
      Spec.assertBool
        s
        (Either.isLeft (Common.defaultedField "k" [] (Common.decodeList Common.asInteger) [Pair.fromString "k" Value.null]))
        "expected a decode failure"
