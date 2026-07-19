-- | Rendering, construction, and extraction for 'Value'. Ported from scrod's
-- JSON encoder (a 'Builder', UTF-8-decoded to 'Text' at the boundary) plus the
-- small tagged-object helpers the codec (§2 of the M3.5 spec) builds on. Parsing
-- is added alongside in a later task.
module Pawl.Json where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Type.Decimal as Decimal
import Pawl.Type.Json (Value)
import qualified Pawl.Type.Json as Json

-- Construction helpers -------------------------------------------------------

jInt :: Integer -> Value
jInt n = Json.Number (Decimal.mkDecimal n 0)

jText :: Text -> Value
jText = Json.String

jBool :: Bool -> Value
jBool = Json.Boolean

tagged :: Text -> Maybe Value -> Value
tagged t mv =
  let base = [(Text.pack "type", Json.String t)]
   in case mv of
        Nothing -> Json.Object base
        Just v -> Json.Object (base <> [(Text.pack "value", v)])

-- Rendering ------------------------------------------------------------------

render :: Value -> Text
render = Encoding.decodeUtf8Lenient . LazyByteString.toStrict . Builder.toLazyByteString . encode

encode :: Value -> Builder.Builder
encode value = case value of
  Json.Null -> Builder.stringUtf8 "null"
  Json.Boolean b -> Builder.stringUtf8 (if b then "true" else "false")
  Json.Number n -> encodeNumber n
  Json.String s -> encodeString s
  Json.Array xs -> surround '[' ']' (map encode xs)
  Json.Object ps -> surround '{' '}' (map encodePair ps)

encodePair :: (Text, Value) -> Builder.Builder
encodePair (k, v) = encodeString k <> Builder.charUtf8 ':' <> encode v

surround :: Char -> Char -> [Builder.Builder] -> Builder.Builder
surround open close items =
  Builder.charUtf8 open <> commaSep items <> Builder.charUtf8 close

commaSep :: [Builder.Builder] -> Builder.Builder
commaSep bs = case bs of
  [] -> mempty
  first : rest -> foldl (\acc b -> acc <> Builder.charUtf8 ',' <> b) first rest

encodeNumber :: Decimal.Decimal -> Builder.Builder
encodeNumber d =
  let e = Decimal.exponent d
   in Builder.integerDec (Decimal.mantissa d)
        <> if e == 0 then mempty else Builder.charUtf8 'e' <> Builder.integerDec e

encodeString :: Text -> Builder.Builder
encodeString s =
  Builder.charUtf8 '"' <> Text.foldr (\c acc -> escape c <> acc) mempty s <> Builder.charUtf8 '"'

escape :: Char -> Builder.Builder
escape c = case c of
  '"' -> Builder.stringUtf8 "\\\""
  '\\' -> Builder.stringUtf8 "\\\\"
  '\n' -> Builder.stringUtf8 "\\n"
  '\r' -> Builder.stringUtf8 "\\r"
  '\t' -> Builder.stringUtf8 "\\t"
  _ ->
    if c < ' '
      then Builder.stringUtf8 ("\\u" <> pad (showHexChar c))
      else Builder.charUtf8 c

pad :: String -> String
pad h = replicate (4 - length h) '0' <> h

showHexChar :: Char -> String
showHexChar c =
  let hexDigit d = Maybe.fromMaybe '0' (lookup d (zip [0 ..] "0123456789abcdef"))
      go n acc =
        if n == 0
          then if null acc then "0" else acc
          else go (div n 16) (hexDigit (mod n 16) : acc)
   in go (fromEnum c) ""

-- Extraction helpers ---------------------------------------------------------

asObject :: Value -> Either Text [(Text, Value)]
asObject value = case value of
  Json.Object ps -> Right ps
  _ -> Left (Text.pack "expected object")

asArray :: Value -> Either Text [Value]
asArray value = case value of
  Json.Array xs -> Right xs
  _ -> Left (Text.pack "expected array")

asText :: Value -> Either Text Text
asText value = case value of
  Json.String s -> Right s
  _ -> Left (Text.pack "expected string")

asInteger :: Value -> Either Text Integer
asInteger value = case value of
  Json.Number d ->
    let e = Decimal.exponent d
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
