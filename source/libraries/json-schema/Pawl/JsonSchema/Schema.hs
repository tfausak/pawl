module Pawl.JsonSchema.Schema where

import qualified Data.Text as Text
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value

-- | A JSON schema, modelled as a plain JSON value.
newtype Schema = MkSchema
  { unwrap :: Value.Value
  }
  deriving (Eq, Show)

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

oneOf :: [Schema] -> Schema
oneOf = fromPairs . pure . Value.pair "oneOf" . Value.array . fmap unwrap

nullable :: Schema -> Schema
nullable s = oneOf [s, Pawl.JsonSchema.Schema.null]

withDefault :: Value.Value -> Schema -> Schema
withDefault v s = fromPairs $ keywords s <> [Value.pair "default" v]
