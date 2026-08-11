-- | A JSON schema, modelled as a plain JSON value. There is more structure
-- available in the specification, but nothing downstream would read it.
--
-- The vocabulary here is generic JSON Schema and knows nothing of Pawl's
-- tagged-object convention, which is composed from these pieces in
-- @Pawl.JsonCodec.Arm@.
module Pawl.JsonSchema.Schema where

import qualified Data.Text as Text
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.Value as Value

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
  v -> [Pair.fromString "allOf" (Value.array [v])]

string :: Schema
string = fromPairs [Pair.fromString "type" (Value.text (Text.pack "string"))]

integer :: Schema
integer = fromPairs [Pair.fromString "type" (Value.text (Text.pack "integer"))]

natural :: Schema
natural =
  fromPairs
    [ Pair.fromString "type" (Value.text (Text.pack "integer")),
      Pair.fromString "minimum" (Value.integer 0)
    ]

null :: Schema
null = fromPairs [Pair.fromString "type" (Value.text (Text.pack "null"))]

constant :: Text.Text -> Schema
constant = fromPairs . pure . Pair.fromString "const" . Value.text

object :: [Pair.Pair Value.Value] -> [Text.Text] -> Schema
object properties required =
  fromPairs
    [ Pair.fromString "type" (Value.text (Text.pack "object")),
      Pair.fromString "properties" (Value.object properties),
      Pair.fromString "required" (Value.array (fmap Value.text required))
    ]

oneOf :: [Schema] -> Schema
oneOf = fromPairs . pure . Pair.fromString "oneOf" . Value.array . fmap unwrap

nullable :: Schema -> Schema
nullable s = oneOf [s, Pawl.JsonSchema.Schema.null]

withDefault :: Value.Value -> Schema -> Schema
withDefault v s = fromPairs $ keywords s <> [Pair.fromString "default" v]
