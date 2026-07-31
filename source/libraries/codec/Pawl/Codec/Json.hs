-- | Construction, normalization, and extraction helpers over the @json@
-- sublibrary's 'Value', plus the small tagged-object convention the codec (§2 of
-- the M3.5 spec) builds on. Encoding and decoding themselves live in
-- 'Pawl.Json.Value'; this module only adapts them to the codec's @Either Text@
-- error channel.
--
-- 'jObject' and 'asObject' trade in assoc lists, which is the shape the codec
-- wants: it writes fields in a readable order rather than an alphabetical one,
-- and reads them back by name. That order is incidental -- JSON objects are
-- unordered, nothing checks the bytes of a card file, and 'sortKeys' exists to
-- compare two values regardless of it.
module Pawl.Codec.Json where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)
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
import Pawl.Json.Value (Value)
import qualified Pawl.Json.Value as Value
import qualified Text.Parsec as Parsec

-- Construction helpers -------------------------------------------------------

jNull :: Value
jNull = Value.Null (Null.MkNull ())

jInt :: Integer -> Value
jInt n = Value.Number (Number.MkNumber (Decimal.mkDecimal n 0))

jText :: Text -> Value
jText = Value.String . String.MkString

jBool :: Bool -> Value
jBool = Value.Boolean . Boolean.MkBoolean

jArray :: [Value] -> Value
jArray = Value.Array . Array.MkArray

jObject :: [(Text, Value)] -> Value
jObject = Value.Object . Object.MkObject . fmap (\(k, v) -> Pair.MkPair (String.MkString k) v)

tagged :: Text -> Maybe Value -> Value
tagged t mv =
  let base = [(Text.pack "type", jText t)]
   in jObject $ case mv of
        Nothing -> base
        Just v -> base <> [(Text.pack "value", v)]

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
sortKeys :: Value -> Value
sortKeys value = case value of
  Value.Array a -> Value.Array (Array.MkArray (fmap sortKeys (Array.unwrap a)))
  Value.Object o ->
    Value.Object
      . Object.MkObject
      . List.sortOn (String.unwrap . Pair.name)
      . fmap (\p -> Pair.MkPair (Pair.name p) (sortKeys (Pair.value p)))
      $ Object.unwrap o
  _ -> value

-- Rendering ------------------------------------------------------------------

render :: Value -> Text
render = Text.pack . Builder.toString . Value.encode

-- Extraction helpers ---------------------------------------------------------

asObject :: Value -> Either Text [(Text, Value)]
asObject value = case value of
  Value.Object o -> Right (fmap (\p -> (String.unwrap (Pair.name p), Pair.value p)) (Object.unwrap o))
  _ -> Left (Text.pack "expected object")

asArray :: Value -> Either Text [Value]
asArray value = case value of
  Value.Array a -> Right (Array.unwrap a)
  _ -> Left (Text.pack "expected array")

asText :: Value -> Either Text Text
asText value = case value of
  Value.String s -> Right (String.unwrap s)
  _ -> Left (Text.pack "expected string")

asInteger :: Value -> Either Text Integer
asInteger value = case value of
  Value.Number n ->
    let d = Number.unwrap n
        e = Decimal.exponent d
     in if e >= 0
          then Right (Decimal.mantissa d * (10 ^ e))
          else Left (Text.pack "expected integer, got fraction")
  _ -> Left (Text.pack "expected number")

field :: Text -> [(Text, Value)] -> Either Text Value
field k ps = case lookup k ps of
  Just v -> Right v
  Nothing -> Left (Text.pack "missing field: " <> k)

optField :: Text -> [(Text, Value)] -> Maybe Value
optField = lookup

tag :: Value -> Either Text (Text, Maybe Value)
tag value = do
  ps <- asObject value
  t <- field (Text.pack "type") ps >>= asText
  pure (t, optField (Text.pack "value") ps)

-- Parsing --------------------------------------------------------------------

-- | 'Value.decode' already consumes the blanks around a document, so this only
-- has to pin the end of input and adapt the error to the codec's channel.
parse :: Text -> Either Text Value
parse input = case Parsec.parse (Value.decode <* Parsec.eof) "" input of
  Left err -> Left (Text.pack (show err))
  Right value -> Right value

-- Collection and default combinators ------------------------------------------
--
-- Generic over the element codec: nothing here names a Pawl.Types type, which
-- is what keeps this module below every per-type codec module (#481).

nullary :: Text -> Value
nullary t = tagged t Nothing

decodeNullary :: Text -> [(Text, a)] -> Value -> Either Text a
decodeNullary tyName table value = do
  (t, _) <- tag value
  case lookup t table of
    Just x -> Right x
    Nothing -> Left (Text.pack "unknown " <> tyName <> Text.pack ": " <> t)

listTo :: (a -> Value) -> [a] -> Value
listTo f = Value.Array . Array.MkArray . fmap f

listFrom :: (Value -> Either Text a) -> Value -> Either Text [a]
listFrom f value = asArray value >>= mapM f

-- CR 613.6's card-data invariant: a static ability has at least one part. An
-- empty array is a decode failure, not an ability that does nothing.
nonEmptyTo :: (a -> Value) -> NonEmpty.NonEmpty a -> Value
nonEmptyTo f = listTo f . NonEmpty.toList

nonEmptyFrom :: (Value -> Either Text a) -> Value -> Either Text (NonEmpty.NonEmpty a)
nonEmptyFrom f value = do
  xs <- listFrom f value
  case NonEmpty.nonEmpty xs of
    Nothing -> Left (Text.pack "expected a non-empty array")
    Just ne -> pure ne

seqTo :: (a -> Value) -> Seq.Seq a -> Value
seqTo f = Value.Array . Array.MkArray . fmap f . Foldable.toList

seqFrom :: (Value -> Either Text a) -> Value -> Either Text (Seq.Seq a)
seqFrom f value = Seq.fromList <$> listFrom f value

setTo :: (a -> Value) -> Set a -> Value
setTo f = listTo f . Set.toAscList

setFrom :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Set a)
setFrom f value = Set.fromList <$> listFrom f value

-- A count-per-key multiset, on the wire as a plain array WITH REPEATS rather
-- than as key/count pairs: it is what the thing being encoded is a list of, and
-- the encoding stays legible beside setTo's. Ascending by key, so it is
-- canonical. multisetFrom recounts, so a hand-written file may repeat a key in
-- any order and a zero count is simply unsayable.
multisetTo :: (a -> Value) -> Map.Map a Natural -> Value
multisetTo f = listTo f . concatMap (\(k, n) -> List.genericReplicate n k) . Map.toAscList

multisetFrom :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Map.Map a Natural)
multisetFrom f value = Map.fromListWith (+) . fmap (\k -> (k, 1)) <$> listFrom f value

maybeTo :: (a -> Value) -> Maybe a -> Value
maybeTo = maybe jNull

maybeFrom :: (Value -> Either Text a) -> Value -> Either Text (Maybe a)
maybeFrom f value = case value of
  Value.Null _ -> Right Nothing
  _ -> Just <$> f value

natTo :: Natural -> Value
natTo = jInt . toInteger

natFrom :: Value -> Either Text Natural
natFrom value = do
  n <- asInteger value
  case Integer.toNatural n of
    Just x -> Right x
    Nothing -> Left (Text.pack "expected natural")

withValue :: Maybe Value -> (Value -> Either Text a) -> Either Text a
withValue mv f = case mv of
  Just v -> f v
  Nothing -> Left (Text.pack "missing tagged value")

-- An omitted delayedAbilities field decodes to empty, so every card file that
-- predates P4 stays byte-identical (the same precedent characteristicPT and
-- colorIndicator follow).
mapFromDefault :: (Value -> Either Text (Map.Map k v)) -> Value -> Either Text (Map.Map k v)
mapFromDefault f value = case value of
  Value.Null _ -> Right Map.empty
  _ -> f value

getOpt :: Text -> [(Text, Value)] -> Value
getOpt k ps = Maybe.fromMaybe jNull (optField k ps)

jsonToBoolDefault :: Bool -> Value -> Either Text Bool
jsonToBoolDefault d value = case value of
  Value.Null _ -> Right d
  Value.Boolean b -> Right (Boolean.unwrap b)
  _ -> Left (Text.pack "expected a boolean")

-- An omitted set field decodes to empty. Lets an all-default field stay OUT of
-- the committed JSON, so existing files remain byte-identical (the same
-- precedent delayedAbilities and characteristicPT follow, P2/P4).
setFromDefault :: (Ord a) => (Value -> Either Text a) -> Value -> Either Text (Set a)
setFromDefault f value = case value of
  Value.Null _ -> Right Set.empty
  _ -> setFrom f value

-- An omitted list field decodes to empty, the list counterpart of
-- setFromDefault. Lets an all-default field stay OUT of the committed JSON, so
-- every existing card file remains byte-identical.
listFromDefault :: (Value -> Either Text a) -> Value -> Either Text [a]
listFromDefault f value = case value of
  Value.Null _ -> Right []
  _ -> listFrom f value
