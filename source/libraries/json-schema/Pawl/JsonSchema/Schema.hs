module Pawl.JsonSchema.Schema where

import qualified Data.Text as Text
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value

-- | A JSON schema, modelled as a plain JSON value.
newtype Schema = MkSchema
  { unwrap :: Value.Value
  }
  deriving (Eq, Ord, Show)

fromPairs :: [Pair.Pair Value.Value] -> Schema
fromPairs = MkSchema . Value.object

-- | A schema's keywords, so another can be added to it. Every schema this
-- module builds is a JSON object -- only the @true@ and @false@ schemas are
-- not, and nothing here makes one -- so the other case is unreachable. Wrapping
-- it in @allOf@ keeps the function total without discarding the schema.
keywords :: Schema -> [Pair.Pair Value.Value]
keywords s = case unwrap s of
  Value.Object o -> Object.unwrap o
  v -> [Value.pair "allOf" $ Value.array [v]]

string :: Schema
string = fromPairs [Value.pair "type" $ Value.string "string"]

integer :: Schema
integer = fromPairs [Value.pair "type" $ Value.string "integer"]

natural :: Schema
natural =
  fromPairs
    [ Value.pair "type" $ Value.string "integer",
      Value.pair "minimum" $ Value.integer 0
    ]

null :: Schema
null = fromPairs [Value.pair "type" $ Value.string "null"]

boolean :: Schema
boolean = fromPairs [Value.pair "type" $ Value.string "boolean"]

array :: Schema -> Schema
array s =
  fromPairs
    [ Value.pair "type" $ Value.string "array",
      Value.pair "items" $ unwrap s
    ]

uniqueArray :: Schema -> Schema
uniqueArray s =
  fromPairs $
    keywords (array s) <> [Value.pair "uniqueItems" $ Value.boolean True]

-- | An array whose decoder rejects an empty one, e.g. 'Pawl.JsonCodec.Common.nonEmpty'.
nonEmptyArray :: Schema -> Schema
nonEmptyArray s = fromPairs $ keywords (array s) <> [Value.pair "minItems" $ Value.integer 1]

tupleOf :: [Schema] -> Schema
tupleOf ss =
  fromPairs
    [ Value.pair "type" $ Value.string "array",
      Value.pair "prefixItems" . Value.array $ fmap unwrap ss,
      Value.pair "minItems" $ Value.integer count,
      Value.pair "maxItems" $ Value.integer count
    ]
  where
    count = toInteger $ length ss

constant :: Text.Text -> Schema
constant = fromPairs . pure . Value.pair "const" . Value.text

object :: [Pair.Pair Value.Value] -> [Text.Text] -> Schema
object properties required =
  fromPairs
    [ Value.pair "type" $ Value.string "object",
      Value.pair "properties" $ Value.object properties,
      Value.pair "required" . Value.array $ fmap Value.text required
    ]

-- | An object used as a MAP: every value shares one shape and the keys are
-- unconstrained, e.g. @Pawl.JsonCodec.Common.textMap@ -- named rather than
-- linked, since pawl:json-schema does not depend on pawl:json-codec. Distinct from
-- 'object', which names its properties and requires some of them; this names
-- none.
--
-- No @propertyNames@. The decoder accepts any string as a key, so constraining
-- keys here would make the schema claim more than the decoder guarantees --- the
-- wrong direction, since the decoder is tightened to honour the schema rather
-- than the schema loosened to describe the decoder.
mapOf :: Schema -> Schema
mapOf s =
  fromPairs
    [ Value.pair "type" $ Value.string "object",
      Value.pair "additionalProperties" $ unwrap s
    ]

-- | An object used as a map whose KEYS are constrained too, e.g. a map keyed
-- by the decimal rendering of a number. Separate from 'mapOf' rather than an
-- argument to it: 'mapOf' says the keys are unconstrained, which is the honest
-- schema for a decoder that accepts any string, and this one is honest only
-- for a decoder that rejects a key the key schema rejects.
mapOfKeys :: Schema -> Schema -> Schema
mapOfKeys k v =
  fromPairs
    [ Value.pair "type" $ Value.string "object",
      Value.pair "propertyNames" $ unwrap k,
      Value.pair "additionalProperties" $ unwrap v
    ]

-- | A string matching a regular expression. 'Pawl.JsonSchema.Pattern' says
-- which patterns the validator can actually check.
matching :: Text.Text -> Schema
matching p = fromPairs $ keywords string <> [Value.pair "pattern" $ Value.text p]

oneOf :: [Schema] -> Schema
oneOf = fromPairs . pure . Value.pair "oneOf" . Value.array . fmap unwrap

nullable :: Schema -> Schema
nullable s = oneOf [s, Pawl.JsonSchema.Schema.null]

withDefault :: Value.Value -> Schema -> Schema
withDefault v s = fromPairs $ keywords s <> [Value.pair "default" v]
