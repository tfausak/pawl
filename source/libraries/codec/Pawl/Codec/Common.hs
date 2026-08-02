-- | Construction, normalization, and extraction helpers over the @json@
-- sublibrary's 'Value.Value', plus the tagged-object convention the codec builds
-- on, the element-generic combinators every per-type codec module is written in
-- terms of, and the assertions its specs are written in terms of. Encoding and
-- decoding themselves live in 'Pawl.Json.Value'; this module adapts them to the
-- codec's @Either Text@ error channel.
--
-- Nothing here names a @Pawl.Types@ type, which is what keeps it below all 98
-- per-type modules rather than in a cycle with them.
--
-- 'object' and 'asObject' trade in 'Pair.Pair' lists, which is the shape the
-- codec wants: it writes fields in a readable order rather than an alphabetical
-- one, and reads them back by name. That order is incidental -- JSON objects are
-- unordered, nothing checks the bytes of a card file, and 'sortKeys' exists to
-- compare two values regardless of it.
module Pawl.Codec.Common where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
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
integer m = number m 0

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
-- key order compare equal. JSON objects are unordered, so this is the right
-- notion of equality for comparing a parsed file against a re-encoded one.
--
-- Arrays are deliberately left alone: JSON arrays /are/ ordered, and the codec
-- relies on that -- a name-keyed map is rendered as a sorted array of entries
-- precisely so the order is deterministic.
--
-- Duplicate keys are not merged. 'List.sortOn' is stable and the extraction
-- helpers take the first match, so the two agree.
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

-- | An absent field reads as JSON null, which is what the @decode*Default@
-- family treats as "say nothing and take the default".
nullableField :: String -> [Pair.Pair Value.Value] -> Value.Value
nullableField k = Maybe.fromMaybe Pawl.Codec.Common.null . lookupPair k

withValue :: Maybe Value.Value -> (Value.Value -> Either Text.Text a) -> Either Text.Text a
withValue mv f = case mv of
  Just v -> f v
  Nothing -> Left $ Text.pack "missing tagged value"

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

-- | Writes a non-empty list as a plain JSON array, the same shape 'encodeList'
-- writes.
encodeNonEmpty :: (a -> Value.Value) -> NonEmpty.NonEmpty a -> Value.Value
encodeNonEmpty f = encodeList f . NonEmpty.toList

-- | The card-data invariant this type exists to enforce: whatever field is
-- typed 'NonEmpty.NonEmpty' has at least one part. An empty array is a decode
-- failure, not a value that does nothing.
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
-- than as key/count pairs: it is what the thing being encoded is a list of, and
-- the encoding stays legible beside encodeSet's. Ascending by key, so it is
-- canonical. decodeMultiset recounts, so a hand-written file may repeat a key in
-- any order and a zero count is simply unsayable.
encodeMultiset :: (a -> Value.Value) -> Map.Map a Natural.Natural -> Value.Value
encodeMultiset f = encodeList f . concatMap (\(k, n) -> List.genericReplicate n k) . Map.toAscList

decodeMultiset :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Map.Map a Natural.Natural)
decodeMultiset f value = Map.fromListWith (+) . fmap (\k -> (k, 1)) <$> decodeList f value

encodeMaybe :: (a -> Value.Value) -> Maybe a -> Value.Value
encodeMaybe = Maybe.maybe Pawl.Codec.Common.null

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

-- Defaults -------------------------------------------------------------------
--
-- An omitted field decodes to the empty or default value, which lets an
-- all-default field stay OUT of the committed JSON so existing card files remain
-- byte-identical.

decodeListDefault :: (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text [a]
decodeListDefault f value = case value of
  Value.Null _ -> Right []
  _ -> decodeList f value

decodeSetDefault :: (Ord a) => (Value.Value -> Either Text.Text a) -> Value.Value -> Either Text.Text (Set.Set a)
decodeSetDefault f value = case value of
  Value.Null _ -> Right Set.empty
  _ -> decodeSet f value

decodeMapDefault :: (Value.Value -> Either Text.Text (Map.Map k v)) -> Value.Value -> Either Text.Text (Map.Map k v)
decodeMapDefault f value = case value of
  Value.Null _ -> Right Map.empty
  _ -> f value

decodeBooleanDefault :: Bool -> Value.Value -> Either Text.Text Bool
decodeBooleanDefault d value = case value of
  Value.Null _ -> Right d
  _ -> asBoolean value

-- Assertions -----------------------------------------------------------------

-- | Asserts both directions of a codec against one JSON literal, which is the
-- shape almost every case in a @Pawl.Codec.XSpec@ takes.
assertJsonCodec :: (Stack.HasCallStack, Monad m, Eq a, Show a) => Spec.Spec m n -> (a -> Value.Value) -> (Value.Value -> Either Text.Text a) -> a -> String -> m ()
assertJsonCodec s enc dec x j = do
  assertToJson s enc x j
  assertFromJson s dec j x

assertFromJson :: (Stack.HasCallStack, Monad m, Eq a, Eq b, Show a, Show b) => Spec.Spec m n -> (Value.Value -> Either a b) -> String -> b -> m ()
assertFromJson s f j x = do
  v <- assertJson s j
  Spec.assertEq s (f v) (Right x)

-- | Compares 'sortKeys'-normalized values, because JSON objects are unordered
-- and key order is not a property the codec has.
assertToJson :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> (a -> Value.Value) -> a -> String -> m ()
assertToJson s f x j = do
  v <- assertJson s j
  Spec.assertEq s (sortKeys (f x)) (sortKeys v)

-- | Goes through 'parse' rather than parsing itself, so a literal with trailing
-- garbage is a test failure instead of silently parsing as its prefix.
assertJson :: (Stack.HasCallStack, Monad m) => Spec.Spec m n -> String -> m Value.Value
assertJson s j = case parse (Text.pack j) of
  Left e -> Spec.assertFailure s $ "invalid JSON: " <> show j <> ": " <> show e
  Right v -> pure v
