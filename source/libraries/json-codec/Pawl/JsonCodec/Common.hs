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
-- short-circuiting, so composing with 'decodeMaybe' accepts an absent key, an
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

-- Combinators ----------------------------------------------------------------
--
-- Generic over the element codec, which is taken as an argument.

decodeNullary :: String -> [(String, a)] -> Value.Value -> Either Text.Text a
decodeNullary tyName table value = do
  (t, _) <- asTagged value
  case lookup t table of
    Just x -> Right x
    Nothing -> Left . Text.pack $ "unknown " <> tyName <> ": " <> t

-- The pairs below (encodeList/decodeList and its siblings) are the last
-- function-shaped combinators; each is waiting to collapse into its
-- Codec-shaped replacement below ('list' and its siblings) during the full
-- pawl:codec conversion (#1263).

encodeList :: (a -> Value.Value) -> [a] -> Value.Value
encodeList f = Value.array . fmap f

decodeList :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text [a]
decodeList f value = asArray value >>= traverse f

encodeNonEmpty :: (a -> Value.Value) -> NonEmpty.NonEmpty a -> Value.Value
encodeNonEmpty f = encodeList f . NonEmpty.toList

-- | An empty array is a decode failure, not a value that does nothing.
decodeNonEmpty :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (NonEmpty.NonEmpty a)
decodeNonEmpty f value = do
  xs <- decodeList f value
  case NonEmpty.nonEmpty xs of
    Nothing -> Left $ Text.pack "expected a non-empty array"
    Just ne -> pure ne

encodeSeq :: (a -> Value.Value) -> Seq.Seq a -> Value.Value
encodeSeq f = encodeList f . Foldable.toList

decodeSeq :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Seq.Seq a)
decodeSeq f value = Seq.fromList <$> decodeList f value

encodeSet :: (a -> Value.Value) -> Set.Set a -> Value.Value
encodeSet f = encodeList f . Set.toAscList

-- | Rejects a repeated element rather than silently collapsing it: a
-- duplicate in a hand-written card file is plausibly a typo, not a value
-- worth accepting, and 'set''s schema says 'Schema.uniqueArray' -- so the
-- decoder has to guarantee what the schema claims.
decodeSet :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Set.Set a)
decodeSet f value = do
  xs <- decodeList f value
  let s = Set.fromList xs
  if Set.size s == length xs
    then Right s
    else Left $ Text.pack "expected an array with no repeated elements"

-- A count-per-key multiset, on the wire as a plain array WITH REPEATS rather
-- than as key/count pairs, ascending by key so it is canonical. decodeMultiset
-- recounts, so a hand-written file may repeat a key in any order and a zero
-- count is unsayable.
encodeMultiset :: (a -> Value.Value) -> Map.Map a Natural.Natural -> Value.Value
encodeMultiset f = encodeList f . concatMap (\(k, n) -> List.genericReplicate n k) . Map.toAscList

decodeMultiset :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Map.Map a Natural.Natural)
decodeMultiset f value = Map.fromListWith (+) . fmap (\k -> (k, 1)) <$> decodeList f value

-- A name-keyed map, on the wire as a JSON OBJECT keyed by the name. Ascending
-- by key, which is canonical and byte-stable for the reason an entry array was
-- once reached for and did not need to be: 'Object.Object' is a LIST OF PAIRS
-- rather than a map, so the order written is the order rendered (#1303).
--
-- The key is a JSON string rather than a 'Value.Value', so these take the key's
-- own wrap and unwrap rather than a codec for it.
encodeTextMap :: (k -> Text.Text) -> (v -> Value.Value) -> Map.Map k v -> Value.Value
encodeTextMap key f =
  Value.object
    . fmap (\(k, v) -> Pair.MkPair (String.MkString (key k)) (f v))
    . Map.toAscList

-- | Rejects a repeated key rather than letting the first win, which is
-- 'decodeSet''s reason: a duplicate in a hand-written card file is plausibly a
-- typo rather than a value worth accepting. A JSON object genuinely can carry
-- one here, since 'Object.Object' does not dedupe.
decodeTextMap ::
  (Ord k) =>
  (Text.Text -> k) ->
  (Value.Value -> Either Text.Text v) ->
  Value.Value ->
  Either Text.Text (Map.Map k v)
decodeTextMap key f value = do
  ps <- asObject value
  entries <- traverse (\p -> fmap ((,) (key (String.unwrap (Pair.name p)))) (f (Pair.value p))) ps
  let m = Map.fromList entries
  if Map.size m == length entries
    then Right m
    else Left $ Text.pack "expected an object with no repeated keys"

encodeMaybe :: (a -> Value.Value) -> Maybe a -> Value.Value
encodeMaybe = Maybe.maybe Value.null

decodeMaybe :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Maybe a)
decodeMaybe f value = case value of
  Value.Null _ -> Right Nothing
  _ -> Just <$> f value

encodeNatural :: Natural.Natural -> Value.Value
encodeNatural = Value.integer . toInteger

decodeNatural :: Value.Value -> Either Text.Text Natural.Natural
decodeNatural value = do
  n <- asInteger value
  case Integer.toNatural n of
    Just x -> Right x
    Nothing -> Left . Text.pack $ "expected natural but got " <> show n

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
-- both hold to it: 'decodeSet' rejects a repeated element so its
-- 'Schema.uniqueArray' schema is honest, and 'decodeNonEmpty' rejects an
-- empty array so its 'Schema.nonEmptyArray' schema is too.

-- | The JSON scalars a wrapper is wrapped around. None of them is filed in
-- @$defs@: a definition is named for a Pawl type, and @Integer@ is not one.
integer :: Codec.Codec Integer
integer = scalar Schema.integer Value.integer asInteger

natural :: Codec.Codec Natural.Natural
natural = scalar Schema.natural encodeNatural decodeNatural

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
    { Codec.encode = encodeMaybe (Codec.encode c),
      Codec.decode = decodeMaybe (Codec.decode c),
      Codec.schema = fmap Schema.nullable (Codec.schema c)
    }

-- The Codec-shaped siblings of encodeList/decodeList and the rest above.
-- 'list', 'set', 'seq', 'nonEmpty' and 'multiset' each wrap their existing
-- function-shaped pair rather than reimplementing it; 'tuple' has no such
-- pair to wrap (see its own Haddock). Each coexists with its function-shaped
-- half until #1263 converts the last caller and deletes it.

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
    { Codec.encode = encodeList (Codec.encode c),
      Codec.decode = decodeList (Codec.decode c),
      Codec.schema = Schema.array <$> Codec.schema c
    }

-- | 'Schema.uniqueArray': both 'Set' on the wire (via 'encodeSet') and
-- 'decodeSet' below reject a repeated element, so the schema saying
-- @uniqueItems@ is a claim the decoder actually guarantees.
set :: (Ord a) => Codec.Codec a -> Codec.Codec (Set.Set a)
set c =
  Codec.MkCodec
    { Codec.encode = encodeSet (Codec.encode c),
      Codec.decode = decodeSet (Codec.decode c),
      Codec.schema = Schema.uniqueArray <$> Codec.schema c
    }

seq :: Codec.Codec a -> Codec.Codec (Seq.Seq a)
seq c =
  Codec.MkCodec
    { Codec.encode = encodeSeq (Codec.encode c),
      Codec.decode = decodeSeq (Codec.decode c),
      Codec.schema = Schema.array <$> Codec.schema c
    }

-- | 'Schema.nonEmptyArray', not 'Schema.array': 'decodeNonEmpty' below rejects
-- an empty array outright, so the schema says the same thing.
nonEmpty :: Codec.Codec a -> Codec.Codec (NonEmpty.NonEmpty a)
nonEmpty c =
  Codec.MkCodec
    { Codec.encode = encodeNonEmpty (Codec.encode c),
      Codec.decode = decodeNonEmpty (Codec.decode c),
      Codec.schema = Schema.nonEmptyArray <$> Codec.schema c
    }

multiset :: (Ord a) => Codec.Codec a -> Codec.Codec (Map.Map a Natural.Natural)
multiset c =
  Codec.MkCodec
    { Codec.encode = encodeMultiset (Codec.encode c),
      Codec.decode = decodeMultiset (Codec.decode c),
      Codec.schema = Schema.array <$> Codec.schema c
    }

-- | A name-keyed map as a JSON object, 'encodeTextMap'\/'decodeTextMap' bundled
-- with 'Schema.mapOf'. The key's unwrap and wrap are passed rather than a
-- @Codec k@, because a JSON object's key is a string rather than a
-- 'Value.Value'; this module cannot name the key type either, since
-- @pawl:json-codec@ does not depend on @pawl:types@.
--
-- The wrap is total. Both key types in the corpus are unvalidated @Text@
-- newtypes, and 'Schema.mapOf' constrains no key, so a fallible wrap would
-- reject documents the schema says are valid.
textMap ::
  (Ord k) =>
  (k -> Text.Text) ->
  (Text.Text -> k) ->
  Codec.Codec v ->
  Codec.Codec (Map.Map k v)
textMap unwrapKey wrapKey c =
  Codec.MkCodec
    { Codec.encode = encodeTextMap unwrapKey (Codec.encode c),
      Codec.decode = decodeTextMap wrapKey (Codec.decode c),
      Codec.schema = Schema.mapOf <$> Codec.schema c
    }

-- | 'assertJsonCodec' against a bundle rather than a loose pair.
assertCodec ::
  (Stack.HasCallStack, Monad m, Eq a, Show a) =>
  Spec.Spec m n ->
  Codec.Codec a ->
  a ->
  String ->
  m ()
assertCodec s c = assertJsonCodec s (Codec.encode c) (Codec.decode c)

-- | Forces a codec's schema and checks only that it is an object. It asserts
-- nothing about the content, so editing a schema never edits a test -- but a
-- bottom fails here, and a definition that fails to terminate fails on the
-- suite's timeout. 'Define.run' applies 'Value.Object' before its list spine
-- is demanded, so pattern-matching the value (as 'asObject' alone does) forces
-- only the outer tag, not the @$defs@ bodies inside it; rendering to text and
-- parsing it back walks the whole tree, which is what actually forces those.
-- Not validated against the schema itself (#1264).
assertHasSchema :: (Stack.HasCallStack, Applicative m) => Spec.Spec m n -> Codec.Codec a -> m ()
assertHasSchema s c =
  Spec.assertBool
    s
    (Either.isRight (asObject =<< parse (render (Define.run (Codec.schema c)))))
    "expected the schema to be an object"
