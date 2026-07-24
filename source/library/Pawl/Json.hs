-- | Rendering, construction, normalization, and extraction for 'Value'. Ported from scrod's
-- JSON encoder (a 'Builder', UTF-8-decoded to 'Text' at the boundary) plus the
-- small tagged-object helpers the codec (§2 of the M3.5 spec) builds on. Parsing
-- is added alongside in a later task.
module Pawl.Json where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Pawl.Type.Decimal as Decimal
import Pawl.Type.Json (Value)
import qualified Pawl.Type.Json as Json
import qualified Text.Parsec as Parsec

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
  Json.Array xs -> Json.Array (fmap sortKeys xs)
  Json.Object ps -> Json.Object (List.sortOn fst (fmap (Bifunctor.second sortKeys) ps))
  _ -> value

-- Rendering ------------------------------------------------------------------

render :: Value -> Text
render = Encoding.decodeUtf8Lenient . LazyByteString.toStrict . Builder.toLazyByteString . encode

encode :: Value -> Builder.Builder
encode value = case value of
  Json.Null -> Builder.stringUtf8 "null"
  Json.Boolean b -> Builder.stringUtf8 (if b then "true" else "false")
  Json.Number n -> encodeNumber n
  Json.String s -> encodeString s
  Json.Array xs -> surround '[' ']' (fmap encode xs)
  Json.Object ps -> surround '{' '}' (fmap encodePair ps)

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

-- Parsing --------------------------------------------------------------------

parse :: Text -> Either Text Value
parse input = case Parsec.parse (whole document) "" input of
  Left err -> Left (Text.pack (show err))
  Right v -> Right v

type P a = Parsec.ParsecT Text () Identity.Identity a

whole :: P a -> P a
whole p = spaces *> p <* spaces <* Parsec.eof

spaces :: P ()
spaces = Parsec.skipMany (Parsec.oneOf " \t\n\r")

document :: P Value
document =
  Parsec.choice
    [ Json.Null <$ Parsec.try (Parsec.string "null"),
      Json.Boolean True <$ Parsec.try (Parsec.string "true"),
      Json.Boolean False <$ Parsec.try (Parsec.string "false"),
      Json.Number <$> pNumber,
      Json.String <$> pString,
      Json.Array <$> pArray,
      Json.Object <$> pObject
    ]

lexeme :: P a -> P a
lexeme p = p <* spaces

pArray :: P [Value]
pArray = Parsec.between (lexeme (Parsec.char '[')) (Parsec.char ']') (Parsec.sepBy (lexeme document) (lexeme (Parsec.char ',')))

pObject :: P [(Text, Value)]
pObject = Parsec.between (lexeme (Parsec.char '{')) (Parsec.char '}') (Parsec.sepBy (lexeme pPair) (lexeme (Parsec.char ',')))

pPair :: P (Text, Value)
pPair = do
  k <- lexeme pString
  _ <- lexeme (Parsec.char ':')
  v <- lexeme document
  pure (k, v)

pString :: P Text
pString = do
  _ <- Parsec.char '"'
  chars <- Parsec.many pChar
  _ <- Parsec.char '"'
  pure (Text.pack chars)

pChar :: P Char
pChar =
  Parsec.choice
    [ Parsec.char '\\' *> pEscape,
      Parsec.satisfy (\c -> c /= '"' && c /= '\\')
    ]

pEscape :: P Char
pEscape =
  Parsec.choice
    [ '"' <$ Parsec.char '"',
      '\\' <$ Parsec.char '\\',
      '/' <$ Parsec.char '/',
      '\n' <$ Parsec.char 'n',
      '\r' <$ Parsec.char 'r',
      '\t' <$ Parsec.char 't',
      '\b' <$ Parsec.char 'b',
      '\f' <$ Parsec.char 'f',
      Parsec.char 'u' *> pUnicode
    ]

pUnicode :: P Char
pUnicode = do
  ds <- Parsec.count 4 Parsec.hexDigit
  pure (toEnum (foldl (\acc d -> acc * 16 + hexVal d) 0 ds))

hexVal :: Char -> Int
hexVal c = Maybe.fromMaybe 0 (lookup c (zip "0123456789abcdefABCDEF" ([0 .. 15] <> [10 .. 15])))

pNumber :: P Decimal.Decimal
pNumber = do
  signF <- Parsec.option id (negate <$ Parsec.char '-')
  intPart <- pInt
  (fracPart, fracExp) <- Parsec.option (0, 0) pFrac
  expPart <- Parsec.option 0 pExp
  pure (Decimal.mkDecimal (signF (intPart * (10 ^ abs fracExp) + fracPart)) (fracExp + expPart))

pInt :: P Integer
pInt =
  Parsec.choice
    [ 0 <$ Parsec.char '0' <* Parsec.notFollowedBy Parsec.digit,
      digitsToInteger <$> ((:) <$> Parsec.satisfy (\c -> c >= '1' && c <= '9') <*> Parsec.many Parsec.digit)
    ]

pFrac :: P (Integer, Integer)
pFrac = do
  ds <- Parsec.char '.' *> Parsec.many1 Parsec.digit
  pure (digitsToInteger ds, negate (toInteger (length ds)))

pExp :: P Integer
pExp = do
  _ <- Parsec.oneOf "eE"
  signF <- Parsec.option id (Parsec.choice [id <$ Parsec.char '+', negate <$ Parsec.char '-'])
  signF . digitsToInteger <$> Parsec.many1 Parsec.digit

digitsToInteger :: String -> Integer
digitsToInteger = foldl (\acc c -> acc * 10 + toInteger (fromEnum c - fromEnum '0')) 0
