{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Pair where

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.String as String
import qualified Text.Parsec as Parsec

data Pair a = MkPair
  { name :: String.String,
    value :: a
  }
  deriving (Eq, Show)

-- | A pair whose key is a literal, which is what every pair a codec or a schema
-- builds has.
fromString :: Prelude.String -> a -> Pair a
fromString = MkPair . String.MkString . Text.pack

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Pair a)
decode p =
  MkPair
    <$> (String.decode <* Parsec.many Array.decodeBlank)
    <*> (Parsec.char ':' *> Parsec.many Array.decodeBlank *> p)

encode :: (a -> Builder.Builder) -> Pair a -> Builder.Builder
encode b p = String.encode (name p) <> Builder.charUtf8 ':' <> b (value p)
