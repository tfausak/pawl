{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Null where

import qualified Data.ByteString.Builder as Builder
import qualified Text.Parsec as Parsec

newtype Null = MkNull
  { unwrap :: ()
  }
  deriving (Eq, Ord, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Null
decode = MkNull () <$ Parsec.string' "null"

encode :: Null -> Builder.Builder
encode = const $ Builder.stringUtf8 "null"
