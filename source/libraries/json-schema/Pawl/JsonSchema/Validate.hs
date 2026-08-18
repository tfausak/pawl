-- | Validates a JSON value against a schema document, so that a codec's
-- encoder can be checked against its own schema rather than reviewed by eye.
--
-- Deliberately NOT a general JSON Schema implementation. It covers exactly the
-- keywords 'Pawl.JsonSchema.Schema' and 'Pawl.JsonSchema.Define' emit, and a
-- keyword outside that set is a FAILURE rather than an annotation to ignore.
-- That inverts the specification's rule on purpose: a conforming validator
-- ignores what it does not understand, which would make this one quietly accept
-- everything a newly added combinator was written to constrain.
module Pawl.JsonSchema.Validate where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Extra.Builder as Builder
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonPointer.Pointer as Pointer
import qualified Pawl.JsonPointer.Token as Token
import qualified Pawl.JsonSchema.Define as Define
import qualified Pawl.JsonSchema.Name as Name

-- | Why a value was rejected, and where in the value it happened. The pointer
-- is rendered rather than kept structured because reading it is the whole
-- point: it is what a maintainer sees when a card file stops matching.
data Failure = MkFailure
  { pointer :: Text.Text,
    message :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | A schema document as 'Define.run' builds one -- the root schema, and the
-- @$defs@ every @$ref@ resolves against -- with the definitions indexed by the
-- reference that reaches each one. Building that index is the expensive half of
-- a validation and does not depend on the value, so validating a corpus
-- 'prepare's once and reuses the result.
data Document = MkDocument
  { root :: Value.Value,
    definitions :: Map.Map Text.Text Value.Value
  }

-- | Indexes a document's definitions by re-encoding each @$defs@ name the way
-- 'Define.reference' writes one, rather than by decoding the reference. That
-- inverts the exact function which produced it, so a name carrying a @/@ or a
-- @~@ that 'Pointer.encode' escapes, or a character that
-- 'Pawl.Uri.Fragment.encode' percent-encodes, resolves without this module
-- owning a decoder for any of it. The card schema really does contain both
-- kinds: @Cost Keyword@ is filed under that name and referenced as
-- @Cost%20Keyword@.
prepare :: Value.Value -> Document
prepare value =
  MkDocument
    { root = value,
      definitions = case value of
        Value.Object o -> case member (Text.pack "$defs") o of
          Just (Value.Object defs) ->
            Map.fromList
              . fmap (\p -> (Define.fragment . Name.MkName . String.unwrap $ Pair.name p, Pair.value p))
              $ Object.unwrap defs
          _ -> Map.empty
        _ -> Map.empty
    }

-- | What a check needs beyond a schema and a value: the document every @$ref@
-- resolves against, where in the value we are, and which references have been
-- followed since the last step INTO the value.
data Context = MkContext
  { document :: Document,
    location :: Pointer.Pointer,
    seen :: Set.Set Text.Text
  }

-- | Validates a value against a whole schema document. An empty list means the
-- value is valid.
validate :: Value.Value -> Value.Value -> [Failure]
validate = validateWith . prepare

-- | 'validate' against an already 'prepare'd document.
validateWith :: Document -> Value.Value -> [Failure]
validateWith d =
  check
    MkContext {document = d, location = Pointer.MkPointer [], seen = Set.empty}
    (root d)

check :: Context -> Value.Value -> Value.Value -> [Failure]
check c schema value = case schema of
  Value.Object o -> concatMap (keyword c o value) $ Object.unwrap o
  _ -> [failure c $ Text.pack "expected a schema object but got " <> render schema]

keyword ::
  Context ->
  Object.Object Value.Value ->
  Value.Value ->
  Pair.Pair Value.Value ->
  [Failure]
keyword c schema value pair =
  let argument = Pair.value pair
   in case Text.unpack . String.unwrap $ Pair.name pair of
        -- The document's own dialect and definition carrier, neither of which
        -- constrains the value.
        "$schema" -> []
        "$defs" -> []
        -- An annotation rather than a constraint: a value differing from the
        -- default is still valid.
        "default" -> []
        "$ref" -> checkRef c argument value
        "type" -> checkType c argument value
        "minimum" -> checkMinimum c argument value
        "const" -> checkConst c argument value
        "items" -> checkItems c schema argument value
        "prefixItems" -> checkPrefixItems c argument value
        "minItems" -> checkItemCount c "at least" (>=) argument value
        "maxItems" -> checkItemCount c "at most" (<=) argument value
        "uniqueItems" -> checkUniqueItems c argument value
        "properties" -> checkProperties c argument value
        "required" -> checkRequired c argument value
        "additionalProperties" -> checkAdditionalProperties c schema argument value
        "oneOf" -> checkOneOf c argument value
        "allOf" -> checkAllOf c argument value
        name -> [failure c $ Text.pack "unknown keyword " <> Text.pack name]

-- | Follows a reference. The context's 'seen' set makes a reference that leads
-- back to itself without ever stepping into the value a failure instead of a
-- hang. It is NOT a visited set over definition names: it is emptied by every
-- step into the value, so an ordinary recursive schema -- a card holding faces
-- holding abilities -- follows the same reference as often as the value nests,
-- and terminates because the value is finite.
checkRef :: Context -> Value.Value -> Value.Value -> [Failure]
checkRef c argument value = case argument of
  Value.String s ->
    let target = String.unwrap s
     in if Set.member target $ seen c
          then [failure c $ Text.pack "cyclic $ref " <> target]
          else case Map.lookup target (definitions (document c)) of
            Nothing -> [failure c $ Text.pack "unresolvable $ref " <> target]
            Just schema -> check c {seen = Set.insert target $ seen c} schema value
  _ -> [failure c $ Text.pack "expected $ref to be a string but got " <> render argument]

checkType :: Context -> Value.Value -> Value.Value -> [Failure]
checkType c argument value = case argument of
  Value.String s -> case hasType (String.unwrap s) value of
    Nothing -> [failure c $ Text.pack "unknown type " <> String.unwrap s]
    Just True -> []
    Just False ->
      [ failure c $
          Text.pack "expected type "
            <> String.unwrap s
            <> Text.pack " but got "
            <> render value
      ]
  _ -> [failure c $ Text.pack "expected type to be a string but got " <> render argument]

-- | 'Nothing' for a type name JSON Schema does not define, which is a schema
-- defect rather than an invalid value.
hasType :: Text.Text -> Value.Value -> Maybe Bool
hasType name value = case Text.unpack name of
  "null" -> Just $ case value of
    Value.Null {} -> True
    _ -> False
  "boolean" -> Just $ case value of
    Value.Boolean {} -> True
    _ -> False
  "string" -> Just $ case value of
    Value.String {} -> True
    _ -> False
  "array" -> Just $ case value of
    Value.Array {} -> True
    _ -> False
  "object" -> Just $ case value of
    Value.Object {} -> True
    _ -> False
  "number" -> Just $ case value of
    Value.Number {} -> True
    _ -> False
  -- 'Decimal.mkDecimal' normalizes by dividing every factor of ten out of the
  -- mantissa and into the exponent, and every number 'Value.number' and
  -- 'Number.decode' build goes through it. So a non-negative exponent is
  -- exactly an integral value, with no rounding through a 'Double' anywhere.
  "integer" -> Just $ case value of
    Value.Number n -> Decimal.exponent (Number.unwrap n) >= 0
    _ -> False
  _ -> Nothing

checkMinimum :: Context -> Value.Value -> Value.Value -> [Failure]
checkMinimum c argument value = case argument of
  Value.Number bound -> case value of
    Value.Number n ->
      if Number.unwrap n >= Number.unwrap bound
        then []
        else
          [ failure c $
              Text.pack "expected a number at least "
                <> render argument
                <> Text.pack " but got "
                <> render value
          ]
    _ -> []
  _ -> [failure c $ Text.pack "expected minimum to be a number but got " <> render argument]

checkConst :: Context -> Value.Value -> Value.Value -> [Failure]
checkConst c argument value =
  if value == argument
    then []
    else
      [ failure c $
          Text.pack "expected the constant "
            <> render argument
            <> Text.pack " but got "
            <> render value
      ]

-- | Applies to the elements a sibling @prefixItems@ does not cover. Nothing
-- this project emits writes both keywords, but they divide an array rather than
-- both covering it, and getting that backwards would silently over-constrain a
-- tuple the day some codec writes the pair.
checkItems :: Context -> Object.Object Value.Value -> Value.Value -> Value.Value -> [Failure]
checkItems c schema argument value = case value of
  Value.Array a ->
    let covered = case member (Text.pack "prefixItems") schema of
          Just (Value.Array p) -> length $ Array.unwrap p
          _ -> 0
     in concatMap
          (\(i, v) -> check (index c i) argument v)
          . drop covered
          . zip [0 ..]
          $ Array.unwrap a
  _ -> []

checkPrefixItems :: Context -> Value.Value -> Value.Value -> [Failure]
checkPrefixItems c argument value = case argument of
  Value.Array schemas -> case value of
    Value.Array a ->
      concat $
        zipWith3
          (check . index c)
          [0 ..]
          (Array.unwrap schemas)
          (Array.unwrap a)
    _ -> []
  _ -> [failure c $ Text.pack "expected prefixItems to be an array but got " <> render argument]

-- | Both @minItems@ and @maxItems@: the same shape with the comparison and the
-- wording swapped.
checkItemCount ::
  Context ->
  String ->
  (Decimal.Decimal -> Decimal.Decimal -> Bool) ->
  Value.Value ->
  Value.Value ->
  [Failure]
checkItemCount c wording op argument value = case argument of
  Value.Number bound -> case value of
    Value.Array a ->
      let count = Decimal.mkDecimal (toInteger . length $ Array.unwrap a) 0
       in if op count $ Number.unwrap bound
            then []
            else
              [ failure c $
                  Text.pack ("expected " <> wording <> " ")
                    <> render argument
                    <> Text.pack " items but got "
                    <> render value
              ]
    _ -> []
  _ -> [failure c $ Text.pack "expected an item count to be a number but got " <> render argument]

checkUniqueItems :: Context -> Value.Value -> Value.Value -> [Failure]
checkUniqueItems c argument value = case argument of
  Value.Boolean b ->
    if not $ Boolean.unwrap b
      then []
      else case value of
        Value.Array a ->
          let items = Array.unwrap a
           in if Set.size (Set.fromList items) == length items
                then []
                else [failure c $ Text.pack "expected unique items but got " <> render value]
        _ -> []
  _ -> [failure c $ Text.pack "expected uniqueItems to be a boolean but got " <> render argument]

-- | Walks the VALUE's properties rather than the schema's, so that an object
-- carrying a name twice has both of its values checked. Duplicate keys are
-- pathological either way; checking every occurrence is the direction that
-- cannot hide one.
checkProperties :: Context -> Value.Value -> Value.Value -> [Failure]
checkProperties c argument value = case argument of
  Value.Object properties -> case value of
    Value.Object o ->
      concatMap
        ( \p ->
            let name = String.unwrap $ Pair.name p
             in case member name properties of
                  Nothing -> []
                  Just schema -> check (into c name) schema $ Pair.value p
        )
        $ Object.unwrap o
    _ -> []
  _ -> [failure c $ Text.pack "expected properties to be an object but got " <> render argument]

checkRequired :: Context -> Value.Value -> Value.Value -> [Failure]
checkRequired c argument value = case argument of
  Value.Array names ->
    concatMap
      ( \n -> case n of
          Value.String s -> case value of
            Value.Object o ->
              if List.any ((== String.unwrap s) . String.unwrap . Pair.name) $ Object.unwrap o
                then []
                else [failure c $ Text.pack "expected the required property " <> String.unwrap s]
            _ -> []
          _ -> [failure c $ Text.pack "expected required to list strings but got " <> render n]
      )
      $ Array.unwrap names
  _ -> [failure c $ Text.pack "expected required to be an array but got " <> render argument]

-- | Applies to the properties a sibling @properties@ does not name.
-- 'Pawl.JsonSchema.Schema.mapOf' writes no @properties@, so today that is every
-- property; reading the sibling anyway keeps the two composable.
checkAdditionalProperties ::
  Context ->
  Object.Object Value.Value ->
  Value.Value ->
  Value.Value ->
  [Failure]
checkAdditionalProperties c schema argument value = case value of
  Value.Object o ->
    let named = case member (Text.pack "properties") schema of
          Just (Value.Object properties) -> fmap (String.unwrap . Pair.name) $ Object.unwrap properties
          _ -> []
     in concatMap
          ( \p ->
              let name = String.unwrap $ Pair.name p
               in if List.elem name named
                    then []
                    else check (into c name) argument $ Pair.value p
          )
          $ Object.unwrap o
  _ -> []

-- | EXACTLY one branch, not at least one. 'Pawl.JsonSchema.Schema.oneOf' is how
-- every tagged union is written, and two branches matching one value means two
-- decoders claim it.
checkOneOf :: Context -> Value.Value -> Value.Value -> [Failure]
checkOneOf c argument value = case argument of
  Value.Array branches ->
    -- Counted through take 2, so a union with many arms stops at the second
    -- match instead of checking the value against every remaining arm.
    let matched = length . take 2 . filter (null . (\schema -> check c schema value)) $ Array.unwrap branches
     in case matched of
          1 -> []
          0 -> [failure c $ Text.pack "no oneOf branch matched " <> render value]
          _ -> [failure c $ Text.pack "more than one oneOf branch matched " <> render value]
  _ -> [failure c $ Text.pack "expected oneOf to be an array but got " <> render argument]

checkAllOf :: Context -> Value.Value -> Value.Value -> [Failure]
checkAllOf c argument value = case argument of
  Value.Array branches -> concatMap (\schema -> check c schema value) $ Array.unwrap branches
  _ -> [failure c $ Text.pack "expected allOf to be an array but got " <> render argument]

-- | Steps into a named property. Emptying 'seen' is what makes the reference
-- guard a loop detector rather than a memo that would wrongly accept.
into :: Context -> Text.Text -> Context
into c step =
  MkContext
    { document = document c,
      location =
        Pointer.MkPointer
          . (<> [Token.MkToken step])
          $ Pointer.unwrap (location c),
      seen = Set.empty
    }

-- | 'into' an array element, whose reference token is its index.
index :: Context -> Integer -> Context
index c = into c . Text.pack . show

member :: Text.Text -> Object.Object Value.Value -> Maybe Value.Value
member name =
  fmap Pair.value
    . List.find ((== name) . String.unwrap . Pair.name)
    . Object.unwrap

failure :: Context -> Text.Text -> Failure
failure c m =
  MkFailure
    { pointer = Text.pack . Builder.toString . Pointer.encode $ location c,
      message = m
    }

-- | 'Pawl.JsonCodec.Common.render' is the same function and is not reachable
-- from here: pawl:json-codec depends on this library rather than the reverse.
render :: Value.Value -> Text.Text
render = Text.pack . Builder.toString . Value.encode
