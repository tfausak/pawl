module Pawl.JsonCodec.CommonSpec where

import qualified Data.Either as Either
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Schema as Schema
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
              [ Value.object [Value.pair "k" (Value.text (Text.pack "v\"x"))],
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
        (Common.sortKeys (Value.object [Value.pair "b" (Value.integer 1), Value.pair "a" (Value.integer 2)]))
        (Value.object [Value.pair "a" (Value.integer 2), Value.pair "b" (Value.integer 1)])
    -- sortKeys is load-bearing for every assertToJson in the codec's specs, so
    -- its own coverage stays thorough.
    Spec.it s "sorts nested objects" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.object [Value.pair "a" (Value.object [Value.pair "d" (Value.integer 2), Value.pair "c" (Value.integer 1)])]))
        (Value.object [Value.pair "a" (Value.object [Value.pair "c" (Value.integer 1), Value.pair "d" (Value.integer 2)])])
    Spec.it s "preserves array order" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.array [Value.integer 2, Value.integer 1]))
        (Value.array [Value.integer 2, Value.integer 1])
    Spec.it s "sorts objects inside arrays" $
      Spec.assertEq
        s
        (Common.sortKeys (Value.array [Value.object [Value.pair "b" (Value.integer 2), Value.pair "a" (Value.integer 1)]]))
        (Value.array [Value.object [Value.pair "a" (Value.integer 1), Value.pair "b" (Value.integer 2)]])
    Spec.it s "leaves an already-sorted value alone" $
      let v = Value.object [Value.pair "a" (Value.integer 1), Value.pair "b" (Value.integer 2)]
       in Spec.assertEq s (Common.sortKeys v) v
    Spec.it s "leaves scalars alone" $
      Spec.assertEq s (Common.sortKeys Value.null) Value.null
    Spec.it s "is idempotent" $
      let v =
            Value.object
              [ Value.pair "b" (Value.object [Value.pair "d" (Value.integer 2), Value.pair "c" (Value.integer 1)]),
                Value.pair "a" Value.null
              ]
       in Spec.assertEq s (Common.sortKeys (Common.sortKeys v)) (Common.sortKeys v)

  Spec.describe s "assertToJson"
    . Spec.it s "ignores object key order"
    $ Common.assertToJson s id (Value.object [Value.pair "b" (Value.integer 1), Value.pair "a" (Value.integer 2)]) "{\"a\":2,\"b\":1}"

  Spec.describe s "defaultedField" $ do
    Spec.it s "supplies the default for an absent key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger []) (Right 0)
    Spec.it s "decodes a present key" $
      Spec.assertEq s (Common.defaultedField "k" (0 :: Integer) Common.asInteger [Value.pair "k" (Value.integer 1)]) (Right 1)
    -- A present null goes to the decoder rather than short-circuiting to the
    -- default, which is what lets decodeMaybe keep accepting an explicit null.
    Spec.it s "hands a present null to the decoder" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (Just (1 :: Integer)) (Codec.decode (Common.maybe Common.integer)) [Value.pair "k" Value.null])
        (Right Nothing)
  Spec.describe s "defaultedField accepts the verbose form" $ do
    -- The key is PRESENT, so 'defaultedField' hands the value straight to the
    -- decoder; the result equals the default only because 'decodeMaybe' reads
    -- an explicit null as Nothing on its own.
    Spec.it s "an explicit null decodes to Nothing via decodeMaybe, not via the default" $
      Spec.assertEq
        s
        (Common.defaultedField "k" Nothing (Codec.decode (Common.maybe Common.integer)) [Value.pair "k" Value.null])
        (Right (Nothing :: Maybe Integer))
    -- Same shape: 'decodeList' on an explicit empty array, not the default.
    Spec.it s "an explicit empty array decodes to [] via decodeList, not via the default" $
      Spec.assertEq
        s
        (Common.defaultedField "k" [] (Codec.decode (Common.list Common.integer)) [Value.pair "k" (Value.array [])])
        (Right ([] :: [Integer]))
    -- An explicit null on a NON-Maybe defaulted field is an error, not the
    -- default: `defaultedField` reads absence directly, so a file that says
    -- `"keywords": null` is malformed rather than empty.
    Spec.it s "an explicit null on a non-Maybe defaulted field is an error" $
      Spec.assertBool
        s
        (Either.isLeft (Common.defaultedField "k" [] (Codec.decode (Common.list Common.integer)) [Value.pair "k" Value.null]))
        "expected a decode failure"

  Spec.describe s "tuple" $ do
    Spec.it s "round trips" $
      Common.assertCodec s (Common.tuple Common.integer Common.integer) (1, 2) "[1,2]"
    Spec.it s "rejects a one-element array" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.tuple Common.integer Common.integer) =<< Common.parse (Text.pack "[1]")))
        "expected a decode failure"
    Spec.it s "rejects a three-element array" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.tuple Common.integer Common.integer) =<< Common.parse (Text.pack "[1,2,3]")))
        "expected a decode failure"

  Spec.describe s "natural"
    . Spec.it s "round trips"
    $ Common.assertCodec s Common.natural 2 "2"

  Spec.describe s "boolean" $ do
    -- Both constructors, since a codec that ignored its argument and always
    -- wrote 'false' would pass on 'False' alone.
    Spec.it s "round trips true" $
      Common.assertCodec s Common.boolean True "true"
    Spec.it s "round trips false" $
      Common.assertCodec s Common.boolean False "false"
    Spec.it s "rejects a non-boolean" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode Common.boolean =<< Common.parse (Text.pack "1")))
        "expected a decode failure"

  Spec.describe s "text" $ do
    -- An escape in the literal, so the case fails an implementation that went
    -- through 'show' rather than the JSON string encoder.
    Spec.it s "round trips" $
      Common.assertCodec s Common.text (Text.pack "a\"b") "\"a\\\"b\""
    Spec.it s "rejects a non-string" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode Common.text =<< Common.parse (Text.pack "1")))
        "expected a decode failure"

  -- Descending and carrying a duplicate, unlike the ascending duplicate-free
  -- literals elsewhere in this file: 'list' preserves order and permits
  -- repeats, and an ascending duplicate-free case would still pass an
  -- implementation that reordered or deduplicated.
  Spec.describe s "list"
    . Spec.it s "round trips"
    $ Common.assertCodec s (Common.list Common.integer) [3, 3, 1] "[3,3,1]"

  Spec.describe s "set" $ do
    Spec.it s "round trips" $
      Common.assertCodec s (Common.set Common.integer) (Set.fromList [1, 2, 3]) "[1,2,3]"
    -- The schema says uniqueItems, so the decoder has to guarantee it: a
    -- repeated element is a decode failure, not a value that silently
    -- collapses.
    Spec.it s "rejects a repeated element on decode" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.set Common.integer) =<< Common.parse (Text.pack "[1,1,2]")))
        "expected a decode failure"

  Spec.describe s "seq"
    . Spec.it s "round trips"
    $ Common.assertCodec s (Common.seq Common.integer) (Seq.fromList [1, 2, 3]) "[1,2,3]"

  Spec.describe s "nonEmpty" $ do
    Spec.it s "round trips" $
      Common.assertCodec s (Common.nonEmpty Common.integer) (NonEmpty.fromList [1, 2, 3]) "[1,2,3]"
    Spec.it s "rejects an empty array" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.nonEmpty Common.integer) =<< Common.parse (Text.pack "[]")))
        "expected a decode failure"

  Spec.describe s "multiset" $ do
    Spec.it s "round trips to ascending order" $
      Common.assertCodec
        s
        (Common.multiset Common.integer)
        (Map.fromList [(1, 2), (2, 1)] :: Map.Map Integer Natural.Natural)
        "[1,1,2]"
    -- decodeMultiset recounts, so the wire order need not match the key order
    -- the encoder would have produced.
    Spec.it s "recounts repeats regardless of input order" $
      Spec.assertEq
        s
        (Codec.decode (Common.multiset Common.integer) =<< Common.parse (Text.pack "[2,1,2]"))
        (Right (Map.fromList [(1, 1), (2, 2)]) :: Either Text.Text (Map.Map Integer Natural.Natural))
  Spec.describe s "textMap" $ do
    -- Written in ascending key order rather than the map's traversal order, so
    -- the render is canonical. The entries are given in DESCENDING order here,
    -- so an encoder that emitted them as it found them fails.
    Spec.it s "encodes in ascending key order" $
      Spec.assertEq
        s
        (Common.render (Codec.encode (Common.textMap id id Common.integer) (Map.fromList [(Text.pack "z", 1), (Text.pack "a", 2)])))
        (Text.pack "{\"a\":2,\"z\":1}")
    Spec.it s "round trips" $
      Spec.assertEq
        s
        (Codec.decode (Common.textMap id id Common.integer) =<< Common.parse (Text.pack "{\"a\":2,\"z\":1}"))
        (Right (Map.fromList [(Text.pack "a", 2), (Text.pack "z", 1)]))
    Spec.it s "decodes the empty object" $
      Spec.assertEq
        s
        (Codec.decode (Common.textMap id id Common.integer) =<< Common.parse (Text.pack "{}"))
        (Right Map.empty :: Either Text.Text (Map.Map Text.Text Integer))
    -- Pawl.Json.Object does not dedupe, so a repeated key genuinely reaches the
    -- decoder. Rejected rather than letting the first win, which is decodeSet's
    -- posture for the same reason.
    Spec.it s "rejects a repeated key" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.textMap id id Common.integer) =<< Common.parse (Text.pack "{\"a\":1,\"a\":2}")))
        "expected a decode failure"
    Spec.it s "rejects an array" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.textMap id id Common.integer) =<< Common.parse (Text.pack "[]")))
        "expected a decode failure"
    -- The bundle carries the pair's behaviour plus a Schema.mapOf schema, which
    -- is the half the loose pair could not supply.
    Spec.it s "the bundle round trips" $
      Common.assertCodec
        s
        (Common.textMap id id Common.integer)
        (Map.fromList [(Text.pack "a", 2), (Text.pack "z", 1)])
        "{\"a\":2,\"z\":1}"
    Spec.it s "the bundle's schema is mapOf over the value schema" $
      Spec.assertEq
        s
        (Define.run (Codec.schema (Common.textMap id id Common.integer)))
        (Define.run (fmap Schema.mapOf (Codec.schema Common.integer)))
