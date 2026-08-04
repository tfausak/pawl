module Pawl.Codec.CommonSpec where

import qualified Data.Either as Either
import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Spec as Spec

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Common" $ do
  Spec.describe s "parse" $ do
    Spec.it s "rejects trailing input" $
      Spec.assertBool s (Either.isLeft . Common.parse $ Text.pack "\"a\" x") "expected a parse failure"
    Spec.it s "round trips through render" $
      Spec.assertEq s (Common.parse (Common.render (Common.array [Common.integer 1]))) (Right (Common.array [Common.integer 1]))
    -- Ported from Pawl.JsonSpec's "nested array/object": a heterogeneous value
    -- (an escaped-quote string, under a nested object, alongside a bool and a
    -- null, all inside an array) round-trips as one composite. The
    -- array-of-integer case above never exercises string escaping, booleans,
    -- null, or a nested object, so this is not a duplicate of it.
    Spec.it s "round trips a nested, heterogeneous value" $
      let v =
            Common.array
              [ Common.object [Common.pair "k" (Common.text (Text.pack "v\"x"))],
                Common.boolean True,
                Common.null
              ]
       in Spec.assertEq s (Common.parse (Common.render v)) (Right v)
    -- Ported from Pawl.JsonSpec's "surrounding blanks are accepted": parse
    -- must accept blanks on both sides of the document, not merely reject
    -- trailing garbage.
    Spec.it s "accepts surrounding blanks" $
      Spec.assertEq s (Common.parse (Text.pack " 1 ")) (Right (Common.integer 1))

  Spec.describe s "tagged" $ do
    Spec.it s "omits an absent value" $
      Spec.assertEq s (Common.render (Common.tagged "ManaValue" Nothing)) (Text.pack "{\"type\":\"ManaValue\"}")
    Spec.it s "includes a present value" $
      Spec.assertEq s (Common.render (Common.tagged "Literal" (Just (Common.integer 5)))) (Text.pack "{\"type\":\"Literal\",\"value\":5}")

  Spec.describe s "asTagged" $ do
    Spec.it s "returns a String tag" $
      Spec.assertEq s (Common.asTagged (Common.nullary "X")) (Right ("X", Nothing))
    -- Ported from Pawl.JsonSpec's "tag reads back what tagged wrote": the
    -- payload-bearing case, which "returns a String tag" above (a nullary tag)
    -- does not exercise.
    Spec.it s "reads back a tagged value's payload" $
      Spec.assertEq s (Common.asTagged (Common.tagged "Literal" (Just (Common.integer 5)))) (Right ("Literal", Just (Common.integer 5)))

  Spec.describe s "sortKeys" $ do
    Spec.it s "orders object keys" $
      Spec.assertEq
        s
        (Common.sortKeys (Common.object [Common.pair "b" (Common.integer 1), Common.pair "a" (Common.integer 2)]))
        (Common.object [Common.pair "a" (Common.integer 2), Common.pair "b" (Common.integer 1)])
    -- The remaining cases are ported from Pawl.JsonSpec's "sortKeys" group:
    -- sortKeys is now load-bearing for every assertToJson in the codec's spec
    -- suite, so its own coverage stays as thorough here as it was there.
    Spec.it s "sorts nested objects" $
      Spec.assertEq
        s
        (Common.sortKeys (Common.object [Common.pair "a" (Common.object [Common.pair "d" (Common.integer 2), Common.pair "c" (Common.integer 1)])]))
        (Common.object [Common.pair "a" (Common.object [Common.pair "c" (Common.integer 1), Common.pair "d" (Common.integer 2)])])
    Spec.it s "preserves array order" $
      Spec.assertEq
        s
        (Common.sortKeys (Common.array [Common.integer 2, Common.integer 1]))
        (Common.array [Common.integer 2, Common.integer 1])
    Spec.it s "sorts objects inside arrays" $
      Spec.assertEq
        s
        (Common.sortKeys (Common.array [Common.object [Common.pair "b" (Common.integer 2), Common.pair "a" (Common.integer 1)]]))
        (Common.array [Common.object [Common.pair "a" (Common.integer 1), Common.pair "b" (Common.integer 2)]])
    Spec.it s "leaves an already-sorted value alone" $
      let v = Common.object [Common.pair "a" (Common.integer 1), Common.pair "b" (Common.integer 2)]
       in Spec.assertEq s (Common.sortKeys v) v
    Spec.it s "leaves scalars alone" $
      Spec.assertEq s (Common.sortKeys Common.null) Common.null
    Spec.it s "is idempotent" $
      let v =
            Common.object
              [ Common.pair "b" (Common.object [Common.pair "d" (Common.integer 2), Common.pair "c" (Common.integer 1)]),
                Common.pair "a" Common.null
              ]
       in Spec.assertEq s (Common.sortKeys (Common.sortKeys v)) (Common.sortKeys v)

  Spec.describe s "assertToJson"
    . Spec.it s "ignores object key order"
    $ Common.assertToJson s id (Common.object [Common.pair "b" (Common.integer 1), Common.pair "a" (Common.integer 2)]) "{\"a\":2,\"b\":1}"

  Spec.describe s "optionalPair" $ do
    Spec.it s "omits a field equal to its default" $
      Spec.assertEq s (Common.optionalPair "k" (0 :: Integer) Common.integer 0) []
    Spec.it s "writes a field differing from its default" $
      Spec.assertEq s (Common.optionalPair "k" (0 :: Integer) Common.integer 1) [Common.pair "k" (Common.integer 1)]
    -- The default is not required to be the type's zero: R2's enum defaults are
    -- ordinary values, and a field equal to one of those is the omitted case.
    Spec.it s "omits a non-zero default" $
      Spec.assertEq s (Common.optionalPair "k" (7 :: Integer) Common.integer 7) []

  Spec.describe s "requiredPair"
    . Spec.it s "always writes the field"
    $ Spec.assertEq s (Common.requiredPair "k" Common.integer (0 :: Integer)) [Common.pair "k" (Common.integer 0)]

  Spec.describe s "defaultedField" $ do
    Spec.it s "supplies the default for an absent key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger []) (Right 0)
    Spec.it s "decodes a present key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger [Common.pair "k" (Common.integer 1)]) (Right 1)
    -- R7: a present null goes to the decoder rather than short-circuiting to the
    -- default, which is what lets decodeMaybe keep accepting an explicit null.
    Spec.it s "hands a present null to the decoder" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (Just (1 :: Integer)) (Common.decodeMaybe Common.asInteger) [Common.pair "k" Common.null])
        (Right Nothing)
    -- The round trip the two halves have to agree on, stated once here so the
    -- per-codec cases in Task 11 are checking a property this pins down.
    Spec.it s "round-trips the default through an omitted field" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (7 :: Integer) Common.asInteger (Common.optionalPair "k" 7 Common.integer 7))
        (Right 7)

  Spec.describe s "defaultedField accepts the verbose form" $ do
    Spec.it s "an explicit null reads as the default for a Maybe" $
      Spec.assertEq
        s
        (Common.defaultedField "k" Nothing (Common.decodeMaybe Common.asInteger) [Common.pair "k" Common.null])
        (Right (Nothing :: Maybe Integer))
    Spec.it s "an explicit empty array reads as the default for a list" $
      Spec.assertEq
        s
        (Common.defaultedField "k" [] (Common.decodeList Common.asInteger) [Common.pair "k" (Common.array [])])
        (Right ([] :: [Integer]))
    -- R7's narrowing, pinned so it cannot drift back by accident: an explicit
    -- null on a NON-Maybe defaulted field is an error, not the default. It used
    -- to be the default only because `nullableField` spelled an absent key as
    -- null; `defaultedField` reads absence directly, so a file that says
    -- `"keywords": null` is malformed rather than empty.
    Spec.it s "an explicit null on a non-Maybe defaulted field is an error" $
      Spec.assertBool
        s
        (Either.isLeft (Common.defaultedField "k" [] (Common.decodeList Common.asInteger) [Common.pair "k" Common.null]))
        "expected a decode failure"
