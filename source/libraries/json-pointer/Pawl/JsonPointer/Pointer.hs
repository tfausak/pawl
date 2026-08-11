{-# LANGUAGE FlexibleContexts #-}

module Pawl.JsonPointer.Pointer where

import qualified Data.Bits as Bits
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Pawl.JsonPointer.Token as Token
import qualified Text.Parsec as Parsec

-- | A JSON Pointer as defined by RFC 6901. A JSON Pointer is a sequence of
-- zero or more reference tokens. An empty list represents the root of the
-- document.
newtype Pointer = MkPointer
  { unwrap :: [Token.Token]
  }
  deriving (Eq, Ord, Show)

-- | Decodes a JSON Pointer from a string. Per RFC 6901, a JSON Pointer is
-- either an empty string (root) or a sequence of reference tokens each
-- prefixed by @/@.
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Pointer
decode = MkPointer <$> Parsec.many (Parsec.char '/' *> Token.decode)

-- | Encodes a JSON Pointer to a string. Each token is prefixed with @/@.
encode :: Pointer -> Builder.Builder
encode = foldMap encodeToken . unwrap

encodeToken :: Token.Token -> Builder.Builder
encodeToken t = Builder.charUtf8 '/' <> Token.encode t

-- | Encodes a JSON Pointer as a URI fragment identifier, per RFC 6901 section
-- 6. Prefixes @#@ and percent-encodes every octet RFC 3986 does not allow in a
-- fragment. It runs over the output of 'encode', so the @~0@ and @~1@ escapes
-- are already in place and survive: @~@ and @\/@ are both legal fragment
-- characters.
encodeFragment :: Pointer -> Builder.Builder
encodeFragment =
  mappend (Builder.charUtf8 '#')
    . foldMap encodeOctet
    . ByteString.unpack
    . Builder.toLazyByteString
    . encode

encodeOctet :: Word.Word8 -> Builder.Builder
encodeOctet w =
  if isFragmentOctet w
    then Builder.word8 w
    else
      let (hi, lo) = divMod w 16
       in Builder.charUtf8 '%' <> Builder.charUtf8 (hexDigit hi) <> Builder.charUtf8 (hexDigit lo)

-- | RFC 3986's @fragment@ production: @unreserved@, @sub-delims@, @:@, @\@@,
-- @\/@ and @?@. Every octet above 127 falls outside it, which is what
-- percent-encodes a UTF-8 sequence byte by byte.
isFragmentOctet :: Word.Word8 -> Bool
isFragmentOctet w =
  let c = Char.chr (word8ToInt w)
   in Char.isAsciiUpper c
        || Char.isAsciiLower c
        || Char.isDigit c
        || elem c "-._~!$&'()*+,;=:@/?"

-- | Uppercase, which RFC 3986 section 2.1 prefers.
hexDigit :: Word.Word8 -> Char
hexDigit w =
  Char.chr $
    if w < 10
      then Char.ord '0' + word8ToInt w
      else Char.ord 'A' + word8ToInt w - 10

-- | Converts a 'Word.Word8' into an 'Int'. Always succeeds: every
-- 'Word.Word8' fits an 'Int'.
word8ToInt :: Word.Word8 -> Int
word8ToInt w = Maybe.fromMaybe 0 (Bits.toIntegralSized w)
