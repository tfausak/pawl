{-# LANGUAGE FlexibleContexts #-}

module Pawl.JsonPointer.Token where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec

-- | A reference token in a JSON Pointer, as defined by RFC 6901. The token
-- stores the unescaped text value.
newtype Token = MkToken
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)

-- | Decodes a reference token from a JSON Pointer string. Handles the escape
-- sequences: @~0 -> ~@, @~1 -> \/@. Per RFC 6901, we must decode @~1@ before
-- @~0@ to avoid errors.
decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Token
decode = MkToken . Text.pack <$> Parsec.many decodeChar

decodeChar :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
decodeChar =
  Parsec.choice
    [ '/' <$ Parsec.string' "~1",
      '~' <$ Parsec.string' "~0",
      Parsec.noneOf "/"
    ]

-- | Encodes a token for use in a JSON Pointer string. Escapes @~@ as @~0@ and
-- @\/@ as @~1@. Per RFC 6901, we must encode @~@ before @\/@ to maintain
-- round-trip consistency.
encode :: Token -> Builder.Builder
encode = foldMap encodeChar . Text.unpack . unwrap

encodeChar :: Char -> Builder.Builder
encodeChar c = case c of
  '~' -> Builder.stringUtf8 "~0"
  '/' -> Builder.stringUtf8 "~1"
  _ -> Builder.charUtf8 c
