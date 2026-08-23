{-# LANGUAGE ScopedTypeVariables #-}

-- | The codec's base module: the tagged-object convention, the element-generic
-- combinators, the field combinators, and the spec assertions every per-type
-- module is written in terms of. Encoding and decoding themselves live in
-- 'Pawl.Json.Value'; this module adapts them to the codec's @Either Text@ error
-- channel.
--
-- 'Pawl.JsonCodec.Fields.defaulted' and 'defaultedField' carry one invariant
-- between them: the
-- default a per-type module passes to each for a given field must be the same
-- binding, or encoding a default value and decoding it back stops being the
-- identity.
--
-- Nothing here names a @Pawl.Types@ type. Since the move to @pawl:json-codec@,
-- that is guaranteed by the build graph rather than by discipline:
-- @pawl:json-codec@ does not depend on @pawl:types@, so a @Pawl.Types@ import
-- would fail to build, which is what keeps this module below the per-type
-- modules rather than in a cycle with them.
--
-- 'Value.object' and 'asObject' trade in 'Pair.Pair' lists so fields can be written
-- in a readable order rather than an alphabetical one. That order is
-- incidental: JSON objects are unordered, and 'sortKeys' exists to compare two
-- values regardless of it.
module Pawl.JsonCodec.Common where

import Control.Monad ((>=>))
import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified GHC.Stack as Stack
import qualified Numeric.Natural as Natural
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Extra.Integer as Integer
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.JsonSchema.Validate as Validate
import qualified Pawl.Spec as Spec
import qualified Text.Parsec as Parsec

-- Tagged objects -------------------------------------------------------------

tagged :: String -> Maybe Value.Value -> Value.Value
tagged t mv =
  Value.object $
    Value.pair "type" (Value.string t) : case mv of
      Nothing -> []
      Just v -> [Value.pair "value" v]

nullary :: String -> Value.Value
nullary t = tagged t Nothing

-- Normalization --------------------------------------------------------------

-- | Recursively sorts every object's keys, so that two values differing only in
-- key order compare equal. Arrays are deliberately left alone: JSON arrays
-- /are/ ordered, and the codec relies on that. Duplicate keys are not merged;
-- 'List.sortOn' is stable and the extraction helpers take the first match, so
-- the two agree.
sortKeys :: Value.Value -> Value.Value
sortKeys value = case value of
  Value.Array a -> Value.Array . Array.MkArray . fmap sortKeys $ Array.unwrap a
  Value.Object o ->
    Value.Object
      . Object.MkObject
      . List.sortOn (String.unwrap . Pair.name)
      . fmap (\p -> Pair.MkPair (Pair.name p) (sortKeys (Pair.value p)))
      $ Object.unwrap o
  _ -> value

-- Rendering and parsing ------------------------------------------------------

render :: Value.Value -> Text.Text
render = Text.pack . Builder.toString . Value.encode

-- | 'Value.decode' already consumes the blanks around a document, so this only
-- has to pin the end of input and adapt the error to the codec's channel.
parse :: Text.Text -> Either Text.Text Value.Value
parse input = case Parsec.parse (Value.decode <* Parsec.eof) "" input of
  Left e -> Left . Text.pack $ show e
  Right value -> Right value

-- Extraction -----------------------------------------------------------------

asObject :: Value.Value -> Either Text.Text [Pair.Pair Value.Value]
asObject v = case v of
  Value.Object o -> Right $ Object.unwrap o
  _ -> Left . Text.pack $ "expected object but got " <> show v

asArray :: Value.Value -> Either Text.Text [Value.Value]
asArray v = case v of
  Value.Array a -> Right $ Array.unwrap a
  _ -> Left . Text.pack $ "expected array but got " <> show v

asText :: Value.Value -> Either Text.Text Text.Text
asText v = case v of
  Value.String s -> Right $ String.unwrap s
  _ -> Left . Text.pack $ "expected string but got " <> show v

asBoolean :: Value.Value -> Either Text.Text Bool
asBoolean v = case v of
  Value.Boolean b -> Right $ Boolean.unwrap b
  _ -> Left . Text.pack $ "expected boolean but got " <> show v

asInteger :: Value.Value -> Either Text.Text Integer
asInteger v = case v of
  Value.Number n ->
    let d = Number.unwrap n
        e = Decimal.exponent d
     in if e >= 0
          then Right $ Decimal.mantissa d * (10 ^ e)
          else Left . Text.pack $ "expected integer but got fraction " <> show v
  _ -> Left . Text.pack $ "expected number but got " <> show v

asTagged :: Value.Value -> Either Text.Text (String, Maybe Value.Value)
asTagged v = do
  ps <- asObject v
  t <- field "type" ps >>= asText
  pure (Text.unpack t, optionalField "value" ps)

-- Fields ---------------------------------------------------------------------

lookupPair :: String -> [Pair.Pair a] -> Maybe a
lookupPair k = fmap Pair.value . List.find ((== Text.pack k) . String.unwrap . Pair.name)

field :: String -> [Pair.Pair Value.Value] -> Either Text.Text Value.Value
field k ps = case lookupPair k ps of
  Just v -> Right v
  Nothing -> Left . Text.pack $ "missing field: " <> k

optionalField :: String -> [Pair.Pair Value.Value] -> Maybe Value.Value
optionalField = lookupPair

withValue :: Maybe Value.Value -> (Value.Value -> Either Text.Text a) -> Either Text.Text a
withValue mv f = case mv of
  Just v -> f v
  Nothing -> Left $ Text.pack "missing tagged value"

-- | Reads a field that may be absent, supplying the default the writer
-- omits. A key that is present but null goes to the decoder rather than
-- short-circuiting, so composing with 'maybe' accepts an absent key, an
-- explicit null, and a value alike.
defaultedField ::
  String ->
  a ->
  (Value.Value -> Either Text.Text a) ->
  [Pair.Pair Value.Value] ->
  Either Text.Text a
defaultedField k d f ps = case lookupPair k ps of
  Nothing -> Right d
  Just v -> f v

-- Assertions -----------------------------------------------------------------

-- | Asserts both directions of a codec against one JSON literal, which is the
-- shape almost every case in a @Pawl.Codec.XSpec@ takes.
assertJsonCodec ::
  (Stack.HasCallStack, Monad m, Eq a, Show a) =>
  Spec.Spec m n ->
  (a -> Value.Value) ->
  (Value.Value -> Either Text.Text a) ->
  a ->
  String ->
  m ()
assertJsonCodec s enc dec x j = do
  assertToJson s enc x j
  assertFromJson s dec j x

assertFromJson ::
  (Stack.HasCallStack, Monad m, Eq a, Eq b, Show a, Show b) =>
  Spec.Spec m n ->
  (Value.Value -> Either a b) ->
  String ->
  b ->
  m ()
assertFromJson s f j x = do
  v <- assertJson s j
  Spec.assertEq s (f v) (Right x)

-- | Compares 'sortKeys'-normalized values, because key order is not a property
-- the codec has. The failure renders both sides as JSON rather than as
-- 'Value.Value', so a mismatch can be pasted back into the literal.
assertToJson ::
  (Stack.HasCallStack, Monad m) =>
  Spec.Spec m n ->
  (a -> Value.Value) ->
  a ->
  String ->
  m ()
assertToJson s f x j = do
  v <- assertJson s j
  Spec.assertBool
    s
    (sortKeys (f x) == sortKeys v)
    ("encoded " <> Text.unpack (render (f x)) <> " but the literal says " <> j)

-- | Goes through 'parse' rather than parsing itself, so a literal with trailing
-- garbage is a test failure instead of silently parsing as its prefix.
assertJson ::
  (Stack.HasCallStack, Monad m) =>
  Spec.Spec m n ->
  String ->
  m Value.Value
assertJson s j = case parse (Text.pack j) of
  Left e -> Spec.assertFailure s $ "invalid JSON: " <> show j <> ": " <> show e
  Right v -> pure v

-- Bundles --------------------------------------------------------------------
--
-- GOVERNING PRINCIPLE: a codec's schema should be as expressive as the
-- constraint it names, and the decoder is tightened to guarantee what the
-- schema claims rather than the other way around. 'set' and 'nonEmpty' below
-- both hold to it: 'set' rejects a repeated element so its
-- 'Schema.uniqueArray' schema is honest, and 'nonEmpty' rejects an empty array
-- so its 'Schema.nonEmptyArray' schema is too.

-- | The JSON scalars a wrapper is wrapped around. None of them is filed in
-- @$defs@: a definition is named for a Pawl type, and @Integer@ is not one.
integer :: Codec.Codec Integer
integer = scalar Schema.integer Value.integer asInteger

natural :: Codec.Codec Natural.Natural
natural =
  scalar
    Schema.natural
    (Value.integer . toInteger)
    ( \value -> do
        n <- asInteger value
        case Integer.toNatural n of
          Just x -> Right x
          Nothing -> Left . Text.pack $ "expected natural but got " <> show n
    )

boolean :: Codec.Codec Bool
boolean = scalar Schema.boolean Value.boolean asBoolean

text :: Codec.Codec Text.Text
text = scalar Schema.string Value.text asText

scalar ::
  Schema.Schema ->
  (a -> Value.Value) ->
  (Value.Value -> Either Text.Text a) ->
  Codec.Codec a
scalar s enc dec = Codec.MkCodec {Codec.encode = enc, Codec.decode = dec, Codec.schema = pure s}

-- | The shape every newtype over another codec's type takes: the same wire
-- format, filed in @$defs@ under the wrapper's own name.
wrapper ::
  forall a b.
  (Typeable.Typeable a) =>
  Codec.Codec b ->
  (b -> a) ->
  (a -> b) ->
  Codec.Codec a
wrapper c inject project =
  Codec.MkCodec
    { Codec.encode = Codec.encode c . project,
      Codec.decode = fmap inject . Codec.decode c,
      Codec.schema = Define.define (Name.typeName (Typeable.Proxy :: Typeable.Proxy a)) (Codec.schema c)
    }

-- | Lifts a codec to one that also reads and writes null. Not filed in @$defs@:
-- @Maybe@ is a structural wrapper rather than a type a reader wants named.
maybe :: Codec.Codec a -> Codec.Codec (Maybe a)
maybe c =
  Codec.MkCodec
    { Codec.encode = Maybe.maybe Value.null (Codec.encode c),
      Codec.decode = \value -> case value of
        Value.Null _ -> Right Nothing
        _ -> Just <$> Codec.decode c value,
      Codec.schema = fmap Schema.nullable (Codec.schema c)
    }

-- The element-generic combinators. Each is ONE bidirectional definition rather
-- than a codec wrapping a separate encode/decode pair: the function-shaped
-- halves this module used to export are gone, their last callers converted
-- (#1263). 'set', 'seq', 'nonEmpty' and 'multiset' are written in terms of
-- 'list', which is where the array itself is read and written.

-- | Encodes to a two-element array and rejects any other length on decode.
-- There is no existing encodeTuple/decodeTuple pair to wrap.
--
-- No caller left in pawl: #1464 gave every positional payload a record. Kept as
-- the worked example of a codec that is not a bare object, which is why it is
-- not deleted along with the three-element version (#1467).
tuple :: Codec.Codec a -> Codec.Codec b -> Codec.Codec (a, b)
tuple ca cb =
  Codec.MkCodec
    { Codec.encode = \(a, b) -> Value.array [Codec.encode ca a, Codec.encode cb b],
      Codec.decode = \value -> do
        xs <- asArray value
        case xs of
          [av, bv] -> (,) <$> Codec.decode ca av <*> Codec.decode cb bv
          _ -> Left . Text.pack $ "expected a 2-element array but got " <> show value,
      Codec.schema = (\a b -> Schema.tupleOf [a, b]) <$> Codec.schema ca <*> Codec.schema cb
    }

list :: Codec.Codec a -> Codec.Codec [a]
list c =
  Codec.MkCodec
    { Codec.encode = Value.array . fmap (Codec.encode c),
      Codec.decode = asArray >=> traverse (Codec.decode c),
      Codec.schema = Schema.array <$> Codec.schema c
    }

-- | 'Schema.uniqueArray', and the decoder guarantees what that claims: a
-- repeated element is REJECTED rather than silently collapsed, because a
-- duplicate in a hand-written card file is plausibly a typo rather than a value
-- worth accepting. Encoding is ascending by element, so the wire form is
-- canonical.
set :: (Ord a) => Codec.Codec a -> Codec.Codec (Set.Set a)
set c =
  Codec.MkCodec
    { Codec.encode = Codec.encode (list c) . Set.toAscList,
      Codec.decode = \value -> do
        xs <- Codec.decode (list c) value
        let s = Set.fromList xs
        if Set.size s == length xs
          then Right s
          else Left $ Text.pack "expected an array with no repeated elements",
      Codec.schema = Schema.uniqueArray <$> Codec.schema c
    }

seq :: Codec.Codec a -> Codec.Codec (Seq.Seq a)
seq c =
  Codec.MkCodec
    { Codec.encode = Codec.encode (list c) . Foldable.toList,
      Codec.decode = fmap Seq.fromList . Codec.decode (list c),
      Codec.schema = Schema.array <$> Codec.schema c
    }

-- | 'Schema.nonEmptyArray', not 'Schema.array', and the decoder says the same
-- thing: an empty array is a decode failure rather than a value that does
-- nothing.
nonEmpty :: Codec.Codec a -> Codec.Codec (NonEmpty.NonEmpty a)
nonEmpty c =
  Codec.MkCodec
    { Codec.encode = Codec.encode (list c) . NonEmpty.toList,
      Codec.decode = \value -> do
        xs <- Codec.decode (list c) value
        case NonEmpty.nonEmpty xs of
          Nothing -> Left $ Text.pack "expected a non-empty array"
          Just ne -> pure ne,
      Codec.schema = Schema.nonEmptyArray <$> Codec.schema c
    }

-- | A key and a value as an OBJECT rather than a two-element array, which is the
-- positional shape #1466 removed.
--
-- Named GENERICALLY, because the helpers built on this are generic and cannot
-- know what their halves mean. A codec that does know says so instead, as
-- Pawl.Codec.EntryRiders' counter pairs do with @kind@ and @count@ -- that one
-- is authored by hand in card files, where the domain names are worth the
-- duplication.
--
-- Written against the primitives rather than Pawl.JsonCodec.Fields, which
-- imports this module.
keyValue :: Codec.Codec k -> Codec.Codec v -> Codec.Codec (k, v)
keyValue ck cv =
  Codec.MkCodec
    { Codec.encode = \(k, v) ->
        Value.object
          [ Value.pair "key" (Codec.encode ck k),
            Value.pair "value" (Codec.encode cv v)
          ],
      Codec.decode = \value -> do
        ps <- asObject value
        k <- Codec.decode ck =<< field "key" ps
        v <- Codec.decode cv =<< field "value" ps
        pure (k, v),
      Codec.schema =
        (\ks vs -> Schema.object [Value.pair "key" (Schema.unwrap ks), Value.pair "value" (Schema.unwrap vs)] [Text.pack "key", Text.pack "value"])
          <$> Codec.schema ck
          <*> Codec.schema cv
    }

-- | A count-per-key multiset, on the wire as an array of key/count objects
-- ascending by key, so it is canonical.
--
-- ONE ENTRY PER KEY, not a plain array with repeats. The repeat form was shorter
-- to write and worse in two ways: a count of twenty wrote its key twenty times,
-- and recounting on decode made a ZERO count unsayable -- which is a state the
-- engine really produces, since Pawl.Engine.Damage takes loyalty and defense
-- counters off with Map.insert and a saturating subtraction rather than pruning
-- the entry (#126).
--
-- A REPEATED key is rejected rather than summed, which is 'set''s and
-- 'keyedList''s reason: the wire form has exactly one way to say each map, so
-- two spellings of one would be a document with no single meaning.
multiset :: (Ord a) => Codec.Codec a -> Codec.Codec (Map.Map a Natural.Natural)
multiset c = keyedList (keyValue c natural)

-- | A map whose ENTRY is spelled by the caller's codec, on the wire as an array
-- of those entries ascending by key, so the encoding is canonical. The entry
-- codec is the caller's because the field names belong to the type being
-- written -- 'Pawl.Codec.EntryRiders' pairs a counter kind with the count an
-- object enters with.
--
-- A repeated key is REJECTED rather than letting one win or combining them,
-- which is 'set''s posture and the one 'multiset' cannot take: there a repeat is
-- how a count is spelled. The schema stays 'Schema.array' and not
-- 'Schema.uniqueArray', because what must be unique is the KEY rather than the
-- whole entry -- two entries sharing a key differ as values, so the decoder is
-- deliberately stricter than the schema here.
keyedList :: (Ord k) => Codec.Codec (k, v) -> Codec.Codec (Map.Map k v)
keyedList c =
  Codec.MkCodec
    { Codec.encode = Codec.encode (list c) . Map.toAscList,
      Codec.decode = \value -> do
        xs <- Codec.decode (list c) value
        let m = Map.fromList xs
        if Map.size m == length xs
          then Right m
          else Left $ Text.pack "expected an array with no repeated keys",
      Codec.schema = Schema.array <$> Codec.schema c
    }

-- | A name-keyed map, on the wire as a JSON OBJECT keyed by the name, ascending
-- by key -- which is canonical and byte-stable because 'Object.Object' is a LIST
-- OF PAIRS rather than a map, so the order written is the order rendered
-- (#1303). A repeated key is REJECTED rather than letting the first win, which
-- is 'set''s reason; a JSON object genuinely can carry one, since
-- 'Object.Object' does not dedupe.
--
-- The key's unwrap and wrap are passed rather than a @Codec k@, because a JSON
-- object's key is a string rather than a 'Value.Value'; this module cannot name
-- the key type either, since @pawl:json-codec@ does not depend on @pawl:types@.
--
-- The wrap is FALLIBLE, because a key that is a rendering of something rather
-- than a bare string can be malformed: see 'naturalMap', whose keys are
-- numbers. A key type that accepts any string recovers the total form as
-- @Right . f@, which is what all three @Text@-newtype call sites pass, so their
-- wire format is unchanged.
textMap ::
  (Ord k) =>
  (k -> Text.Text) ->
  (Text.Text -> Either Text.Text k) ->
  Codec.Codec v ->
  Codec.Codec (Map.Map k v)
textMap unwrapKey wrapKey c =
  Codec.MkCodec
    { Codec.encode =
        Value.object
          . fmap (\(k, v) -> Pair.MkPair (String.MkString (unwrapKey k)) (Codec.encode c v))
          . Map.toAscList,
      Codec.decode = \value -> do
        ps <- asObject value
        entries <- traverse (\p -> (,) <$> wrapKey (String.unwrap (Pair.name p)) <*> Codec.decode c (Pair.value p)) ps
        let m = Map.fromList entries
        if Map.size m == length entries
          then Right m
          else Left $ Text.pack "expected an object with no repeated keys",
      Codec.schema = Schema.mapOf <$> Codec.schema c
    }

-- | A map keyed by a NUMBER, on the wire as a JSON object whose keys are the
-- decimal renderings of those numbers -- which is the class that dominates a
-- game state, so its readability is what decides whether a written-out state is
-- diffable.
--
-- The key functions are derived from the key type's OWN codec rather than
-- hand-written, so the key's wire shape stays tied to the codec that already
-- defines it and the fallibility falls out of 'Codec.decode'. That derivation
-- is not applied uniformly to every key type: a string key would come back from
-- its codec as a quoted JSON string, and rendering THAT would key the object by
-- @\"a\"@ rather than by @a@. So 'textMap' is the string case and this is the
-- number case, per scalar kind rather than per codec.
--
-- The schema constrains the keys, which is what lets the wrap reject one:
-- 'Schema.mapOf' would claim any string is a key while this decoder rejects
-- most of them.
naturalMap ::
  (Ord k) =>
  Codec.Codec k ->
  Codec.Codec v ->
  Codec.Codec (Map.Map k v)
naturalMap ck cv =
  (textMap (naturalKeyText ck) (naturalKeyValue ck) cv)
    { Codec.schema = Schema.mapOfKeys (Schema.matching naturalKeyPattern) <$> Codec.schema cv
    }

-- | What a 'naturalMap' key looks like: a decimal natural with no leading zero,
-- no sign and no exponent.
naturalKeyPattern :: Text.Text
naturalKeyPattern = Text.pack "^(0|[1-9][0-9]*)$"

-- | Encodes a key through its own codec and renders the resulting scalar as a
-- decimal integer. Rendering the scalar as JSON would not do: 'Value.encode'
-- writes a normalized decimal, so an object id of 100 would come out as
-- @1e2@ -- one JSON number, but not the key string a reader expects.
--
-- Total, because 'textMap' takes a total unwrap. A key codec that writes
-- something other than an integer falls back to the JSON rendering, which
-- cannot match 'naturalKeyPattern', so 'assertMatchesSchema' catches it rather
-- than the wire format quietly changing shape.
naturalKeyText :: Codec.Codec k -> k -> Text.Text
naturalKeyText c k =
  let value = Codec.encode c k
   in case asInteger value of
        Right n -> Text.pack $ show n
        Left _ -> render value

-- | The other half of the derivation: parse the key string as a JSON scalar and
-- hand it to the same codec, so @{"abc": ...}@ is REJECTED rather than folded
-- to a silent @0@ that would then collide with a real key.
--
-- No format check beyond what the codec does. A non-canonical spelling such as
-- @1e0@ decodes to the same key as @1@, and 'textMap''s repeated-key check is
-- what stops the two collapsing onto one entry; the schema's @pattern@ is what
-- states the canonical shape. So the decoder is a shade LOOSER than the schema
-- rather than stricter, which is the safe direction: nothing the schema accepts
-- is rejected, which is what 'textMap''s comment refuses to allow.
naturalKeyValue :: Codec.Codec k -> Text.Text -> Either Text.Text k
naturalKeyValue c = parse >=> Codec.decode c

-- | 'assertJsonCodec' against a bundle rather than a loose pair, plus the
-- assertion only a bundle can make: that the encoder writes what the schema
-- claims. The schema assertion runs FIRST so that nothing ahead of it can
-- absorb a schema defect and report itself instead.
assertCodec ::
  (Stack.HasCallStack, Monad m, Eq a, Show a) =>
  Spec.Spec m n ->
  Codec.Codec a ->
  a ->
  String ->
  m ()
assertCodec s c x j = do
  assertMatchesSchema s c x
  assertJsonCodec s (Codec.encode c) (Codec.decode c) x j

-- | Validates a codec's own encoding of a value against its own schema. This is
-- the pairing 'assertHasSchema' cannot make: a schema that renders is not a
-- schema that describes what the encoder writes.
assertMatchesSchema ::
  (Stack.HasCallStack, Applicative m) =>
  Spec.Spec m n ->
  Codec.Codec a ->
  a ->
  m ()
assertMatchesSchema s c x =
  Spec.assertEqWith
    s
    "schema"
    (Validate.validate (Define.run (Codec.schema c)) (Codec.encode c x))
    []

-- | Round-trips EVERY constructor of an all-nullary type through its codec, and
-- asserts that no two of them encode alike.
--
-- The exhaustive counterpart to the hand-written literal assertions beside it,
-- which are representative rather than complete -- Pawl.Codec.SubtypeSpec covers
-- a fraction of its type. What it pins is the property 'Pawl.JsonCodec.Arm.enum'
-- actually rests on: every constructor survives the trip, and the derived tags
-- are distinct. It cannot pin the tag STRINGS without restating 'show', which is
-- what the literal assertions are for.
assertEnumCodec ::
  forall m n a.
  (Stack.HasCallStack, Monad m, Bounded a, Enum a, Eq a, Show a) =>
  Spec.Spec m n ->
  Codec.Codec a ->
  m ()
assertEnumCodec s c = do
  let values = [minBound .. maxBound] :: [a]
      encoded = fmap (render . Codec.encode c) values
  Spec.assertEq s (traverse (Codec.decode c . Codec.encode c) values) (Right values)
  Spec.assertEq s (length (Set.fromList encoded)) (length encoded)

-- | Forces a codec's schema and checks only that it is an object. It asserts
-- nothing about the content, so editing a schema never edits a test -- but a
-- bottom fails here, and a definition that fails to terminate fails on the
-- suite's timeout. 'Define.run' applies 'Value.Object' before its list spine
-- is demanded, so pattern-matching the value (as 'asObject' alone does) forces
-- only the outer tag, not the @$defs@ bodies inside it; rendering to text and
-- parsing it back walks the whole tree, which is what actually forces those.
-- What the schema SAYS is checked by 'assertMatchesSchema', against a value the
-- codec encoded.
assertHasSchema :: (Stack.HasCallStack, Applicative m) => Spec.Spec m n -> Codec.Codec a -> m ()
assertHasSchema s c =
  Spec.assertBool
    s
    (Either.isRight (asObject =<< parse (render (Define.run (Codec.schema c)))))
    "expected the schema to be an object"
