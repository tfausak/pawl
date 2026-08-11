-- | A JSON schema, modelled as a plain JSON value. There is more structure
-- available in the specification, but nothing downstream would read it.
--
-- The vocabulary here is generic JSON Schema and knows nothing of Pawl's
-- tagged-object convention, which is composed from these pieces in
-- @Pawl.JsonCodec.Arm@.
module Pawl.JsonSchema.Schema where

import qualified Data.Text as Text
import qualified Pawl.Decimal as Decimal
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.Pair as Pair
import qualified Pawl.Json.String as String
import qualified Pawl.Json.Value as Value

newtype Schema = MkSchema
  { unwrap :: Value.Value
  }
  deriving (Eq, Show)

pair :: String -> Value.Value -> Pair.Pair Value.Value
pair = Pair.MkPair . String.MkString . Text.pack

text :: Text.Text -> Value.Value
text = Value.String . String.MkString

integerValue :: Integer -> Value.Value
integerValue = Value.Number . Number.MkNumber . flip Decimal.mkDecimal 0

fromPairs :: [Pair.Pair Value.Value] -> Schema
fromPairs = MkSchema . Value.Object . Object.MkObject

-- | A schema's keywords, so another can be added to it. Every schema this
-- module builds is a JSON object -- only the @true@ and @false@ schemas are
-- not, and nothing here makes one -- so the other case is unreachable. Wrapping
-- it in @allOf@ keeps the function total without discarding the schema.
keywords :: Schema -> [Pair.Pair Value.Value]
keywords s = case unwrap s of
  Value.Object o -> Object.unwrap o
  v -> [pair "allOf" (Value.Array (Array.MkArray [v]))]

string :: Schema
string = fromPairs [pair "type" (text (Text.pack "string"))]

integer :: Schema
integer = fromPairs [pair "type" (text (Text.pack "integer"))]

natural :: Schema
natural =
  fromPairs
    [ pair "type" (text (Text.pack "integer")),
      pair "minimum" (integerValue 0)
    ]

null :: Schema
null = fromPairs [pair "type" (text (Text.pack "null"))]

constant :: Text.Text -> Schema
constant = fromPairs . pure . pair "const" . text

object :: [Pair.Pair Value.Value] -> [Text.Text] -> Schema
object properties required =
  fromPairs
    [ pair "type" (text (Text.pack "object")),
      pair "properties" (Value.Object (Object.MkObject properties)),
      pair "required" (Value.Array (Array.MkArray (fmap text required)))
    ]

oneOf :: [Schema] -> Schema
oneOf = fromPairs . pure . pair "oneOf" . Value.Array . Array.MkArray . fmap unwrap

nullable :: Schema -> Schema
nullable s = oneOf [s, Pawl.JsonSchema.Schema.null]

withDefault :: Value.Value -> Schema -> Schema
withDefault v s = fromPairs $ keywords s <> [pair "default" v]
