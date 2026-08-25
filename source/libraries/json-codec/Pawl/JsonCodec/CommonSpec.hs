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
import qualified Pawl.JsonSchema.Validate as Validate
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
    -- default, which is what lets Common.maybe keep accepting an explicit null.
    Spec.it s "hands a present null to the decoder" $
      Spec.assertEq
        s
        (Common.defaultedField "k" (Just (1 :: Integer)) (Codec.decode (Common.maybe Common.integer)) [Value.pair "k" Value.null])
        (Right Nothing)
  Spec.describe s "defaultedField accepts the verbose form" $ do
    -- The key is PRESENT, so 'defaultedField' hands the value straight to the
    -- decoder; the result equals the default only because Common.maybe reads
    -- an explicit null as Nothing on its own.
    Spec.it s "an explicit null decodes to Nothing via Common.maybe, not via the default" $
      Spec.assertEq
        s
        (Common.defaultedField "k" Nothing (Codec.decode (Common.maybe Common.integer)) [Value.pair "k" Value.null])
        (Right (Nothing :: Maybe Integer))
    -- Same shape: Common.list on an explicit empty array, not the default.
    Spec.it s "an explicit empty array decodes to [] via Common.list, not via the default" $
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

  Spec.describe s "nonEmptyKeyedList" $ do
    Spec.it s "round trips" $
      Common.assertCodec s (Common.nonEmptyKeyedList (Common.keyValue Common.integer Common.integer)) (Map.fromList [(1, 2)]) "[{\"key\":1,\"value\":2}]"
    Spec.it s "rejects an empty array" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.nonEmptyKeyedList (Common.keyValue Common.integer Common.integer)) =<< Common.parse (Text.pack "[]")))
        "expected a decode failure"

  Spec.describe s "multiset" $ do
    -- ONE entry per key, ascending, and the counts are given DESCENDING here so
    -- an encoder that emitted the map in traversal order would fail.
    Spec.it s "round trips to ascending order" $
      Common.assertCodec
        s
        (Common.multiset Common.integer)
        (Map.fromList [(1, 2), (2, 1)] :: Map.Map Integer Natural.Natural)
        "[{\"key\":1,\"value\":2},{\"key\":2,\"value\":1}]"
    -- A ZERO count is sayable, which the repeat encoding this replaced could not
    -- express at all: Pawl.Engine.Damage spends a shield counter with Map.insert
    -- rather than pruning the entry, so a game state really holds one (#126).
    Spec.it s "keeps a zero count" $
      Common.assertCodec
        s
        (Common.multiset Common.integer)
        (Map.fromList [(1, 0)] :: Map.Map Integer Natural.Natural)
        "[{\"key\":1,\"value\":0}]"
    -- Rejected rather than summed, which is 'set''s reason: one map, one
    -- spelling.
    Spec.it s "rejects a repeated key" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.multiset Common.integer) =<< Common.parse (Text.pack "[{\"key\":1,\"value\":1},{\"key\":1,\"value\":2}]")))
        "expected a decode failure"
  Spec.describe s "textMap" $ do
    -- Written in ascending key order rather than the map's traversal order, so
    -- the render is canonical. The entries are given in DESCENDING order here,
    -- so an encoder that emitted them as it found them fails.
    Spec.it s "encodes in ascending key order" $
      Spec.assertEq
        s
        (Common.render (Codec.encode (Common.textMap id Right Common.integer) (Map.fromList [(Text.pack "z", 1), (Text.pack "a", 2)])))
        (Text.pack "{\"a\":2,\"z\":1}")
    Spec.it s "round trips" $
      Spec.assertEq
        s
        (Codec.decode (Common.textMap id Right Common.integer) =<< Common.parse (Text.pack "{\"a\":2,\"z\":1}"))
        (Right (Map.fromList [(Text.pack "a", 2), (Text.pack "z", 1)]))
    Spec.it s "decodes the empty object" $
      Spec.assertEq
        s
        (Codec.decode (Common.textMap id Right Common.integer) =<< Common.parse (Text.pack "{}"))
        (Right Map.empty :: Either Text.Text (Map.Map Text.Text Integer))
    -- Pawl.Json.Object does not dedupe, so a repeated key genuinely reaches the
    -- decoder. Rejected rather than letting the first win, which is Common.set's
    -- posture for the same reason.
    Spec.it s "rejects a repeated key" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.textMap id Right Common.integer) =<< Common.parse (Text.pack "{\"a\":1,\"a\":2}")))
        "expected a decode failure"
    Spec.it s "rejects an array" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode (Common.textMap id Right Common.integer) =<< Common.parse (Text.pack "[]")))
        "expected a decode failure"
    -- The bundle carries the pair's behaviour plus a Schema.mapOf schema, which
    -- is the half the loose pair could not supply.
    Spec.it s "the bundle round trips" $
      Common.assertCodec
        s
        (Common.textMap id Right Common.integer)
        (Map.fromList [(Text.pack "a", 2), (Text.pack "z", 1)])
        "{\"a\":2,\"z\":1}"
    Spec.it s "the bundle's schema is mapOf over the value schema" $
      Spec.assertEq
        s
        (Define.run (Codec.schema (Common.textMap id Right Common.integer)))
        (Define.run (fmap Schema.mapOf (Codec.schema Common.integer)))

  Spec.describe s "naturalMap" $ do
    let codec = Common.naturalMap Common.natural Common.integer
        entries = Map.fromList [(2, 3), (10, 4)] :: Map.Map Natural.Natural Integer
    -- Keys 2 and 10 sort the other way as TEXT, so an encoder that ordered the
    -- keys by their renderings rather than by the map fails here. 100 would
    -- render as 1e2 if the key went through Value.encode.
    Spec.it s "keys by the decimal rendering, ascending numerically" $
      Spec.assertEq
        s
        (Common.render (Codec.encode codec (Map.insert 100 5 entries)))
        (Text.pack "{\"2\":3,\"10\":4,\"100\":5}")
    Spec.it s "the bundle round trips" $
      Common.assertCodec s codec entries "{\"2\":3,\"10\":4}"
    -- The mutation target: a total wrap would fold this to a key rather than
    -- rejecting it, and no round trip can catch that because the encoder never
    -- writes such a key.
    Spec.it s "rejects a key that is not a number" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode codec =<< Common.parse (Text.pack "{\"abc\":1}")))
        "expected a decode failure"
    Spec.it s "rejects a key that is a number the key codec refuses" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode codec =<< Common.parse (Text.pack "{\"-1\":1}")))
        "expected a decode failure"
    -- Two spellings of one number are one KEY, so this is the repeated-key
    -- check doing the work no canonical-format check was added for.
    Spec.it s "rejects two spellings of one key" $
      Spec.assertBool
        s
        (Either.isLeft (Codec.decode codec =<< Common.parse (Text.pack "{\"1\":1,\"1e0\":2}")))
        "expected a decode failure"
    Spec.it s "the schema constrains the keys to the decimal pattern" $
      Spec.assertEq
        s
        (Define.run (Codec.schema codec))
        (Define.run (fmap (Schema.mapOfKeys (Schema.matching Common.naturalKeyPattern)) (Codec.schema Common.integer)))
    -- The pairing the schema equality cannot make: that the published schema
    -- actually REJECTS the key the decoder rejects, so the two agree in the
    -- direction Common.textMap's comment cares about.
    Spec.it s "the schema rejects a key that is not a number" $
      Spec.assertBool
        s
        ( Either.either
            (const False)
            (not . null . Validate.validate (Define.run (Codec.schema codec)))
            (Common.parse (Text.pack "{\"abc\":1}"))
        )
        "expected the schema to reject the key"
    Spec.it s "the schema accepts what the encoder writes" $
      Spec.assertEq
        s
        (Validate.validate (Define.run (Codec.schema codec)) (Codec.encode codec entries))
        []

  Spec.describe s "assertMatchesSchema" $ do
    -- A capturing spec, so that a FAILING assertion is a value rather than the
    -- end of the run. Pawl.Spec's assertFailure is polymorphic in its result,
    -- which makes Either's Left a lawful implementation of it.
    let capture =
          Spec.MkSpec
            { Spec.assertFailure = Left,
              Spec.describe = \_ x -> x,
              Spec.it = \_ x -> x
            }
        -- Deliberately self-contradictory: the encoder writes a number where
        -- the schema says string. No codec built from the combinators above can
        -- be, which is why this one is assembled by hand.
        inconsistent =
          Codec.MkCodec
            { Codec.encode = \() -> Value.integer 1,
              Codec.decode = \_ -> Right (),
              Codec.schema = pure Schema.string
            }
    Spec.it s "passes a codec that agrees with its schema" $
      Spec.assertEq s (Common.assertMatchesSchema capture Common.integer 1) (Right ())
    Spec.it s "fails a codec whose encoder contradicts its schema" $
      Spec.assertBool
        s
        (Either.isLeft (Common.assertMatchesSchema capture inconsistent ()))
        "expected the assertion to fail"
    -- The wiring rather than the helper: assertCodec is what every per-type
    -- spec calls, so dropping its schema step would leave this green.
    Spec.it s "assertCodec checks the schema" $
      Spec.assertBool
        s
        (Either.isLeft (Common.assertCodec capture inconsistent () "1"))
        "expected the assertion to fail"
