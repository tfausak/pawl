{-# LANGUAGE ScopedTypeVariables #-}

-- | The codec's base module: the tagged-object convention, the element-generic
-- combinators, the field combinators, and the spec assertions every per-type
-- module is written in terms of. Encoding and decoding themselves live in
-- 'Pawl.Json.Value'; this module adapts them to the codec's @Either Text@ error
-- channel.
--
-- 'optionalPair' and 'defaultedField' carry one invariant between them: the
-- default a per-type module passes to each for a given field must be the same
-- binding, or encoding a default value and decoding it back stops being the
-- identity.
--
-- Nothing here names a @Pawl.Types@ type, which is what keeps it below the
-- per-type modules rather than in a cycle with them.
--
-- 'object' and 'asObject' trade in 'Pair.Pair' lists so fields can be written
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
import qualified Pawl.Json.Null as Null
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

-- Construction ---------------------------------------------------------------

null :: Value.Value
null = Value.Null $ Null.MkNull ()

boolean :: Bool -> Value.Value
boolean = Value.Boolean . Boolean.MkBoolean

number :: Integer -> Integer -> Value.Value
number m = Value.Number . Number.MkNumber . Decimal.mkDecimal m

-- | The whole-number case of 'number', which is every number the codec writes.
integer :: Integer -> Value.Value
integer = flip number 0

string :: String -> Value.Value
string = text . Text.pack

text :: Text.Text -> Value.Value
text = Value.String . String.MkString

array :: [Value.Value] -> Value.Value
array = Value.Array . Array.MkArray

pair :: String -> a -> Pair.Pair a
pair = Pair.MkPair . String.MkString . Text.pack

object :: [Pair.Pair Value.Value] -> Value.Value
object = Value.Object . Object.MkObject

-- Tagged objects -------------------------------------------------------------

tagged :: String -> Maybe Value.Value -> Value.Value
tagged t mv =
  object $
    pair "type" (string t) : case mv of
      Nothing -> []
      Just v -> [pair "value" v]

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

-- | A field that is always written, whatever its value. The singleton list is
-- so that 'Common.object . concat' can take required and optional fields in one
-- list, with which is which readable down the left edge.
requiredPair :: String -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]
requiredPair k f x = [pair k (f x)]

-- | A field written only when it differs from the default that an absent key
-- means. The default passed here and the one 'defaultedField' supplies must be
-- the same binding.
optionalPair :: (Eq a) => String -> a -> (a -> Value.Value) -> a -> [Pair.Pair Value.Value]
optionalPair k d f x = if x == d then [] else [pair k (f x)]

-- | Reads a field that may be absent, supplying the default 'optionalPair'
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

encodeList :: (a -> Value.Value) -> [a] -> Value.Value
encodeList f = array . fmap f

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

decodeSet :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Set.Set a)
decodeSet f value = Set.fromList <$> decodeList f value

-- A count-per-key multiset, on the wire as a plain array WITH REPEATS rather
-- than as key/count pairs, ascending by key so it is canonical. decodeMultiset
-- recounts, so a hand-written file may repeat a key in any order and a zero
-- count is unsayable.
encodeMultiset :: (a -> Value.Value) -> Map.Map a Natural.Natural -> Value.Value
encodeMultiset f = encodeList f . concatMap (\(k, n) -> List.genericReplicate n k) . Map.toAscList

decodeMultiset :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Map.Map a Natural.Natural)
decodeMultiset f value = Map.fromListWith (+) . fmap (\k -> (k, 1)) <$> decodeList f value

encodeMaybe :: (a -> Value.Value) -> Maybe a -> Value.Value
encodeMaybe = Maybe.maybe Pawl.JsonCodec.Common.null

decodeMaybe :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Maybe a)
decodeMaybe f value = case value of
  Value.Null _ -> Right Nothing
  _ -> Just <$> f value

encodeNatural :: Natural.Natural -> Value.Value
encodeNatural = integer . toInteger

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

-- | The shape every newtype over a JSON scalar takes: a fixed schema, filed
-- under the type's own name, with the encoder and decoder passed in.
scalar ::
  forall a.
  (Typeable.Typeable a) =>
  Schema.Schema ->
  (a -> Value.Value) ->
  (Value.Value -> Either Text.Text a) ->
  Codec.Codec a
scalar s enc dec =
  Codec.MkCodec
    { Codec.encode = enc,
      Codec.decode = dec,
      Codec.schema = Define.define (Name.typeName (Typeable.Proxy :: Typeable.Proxy a)) (pure s)
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
assertHasSchema :: (Stack.HasCallStack, Applicative m) => Spec.Spec m n -> Codec.Codec a -> m ()
assertHasSchema s c =
  Spec.assertBool
    s
    (Either.isRight (asObject =<< parse (render (Define.run (Codec.schema c)))))
    "expected the schema to be an object"
