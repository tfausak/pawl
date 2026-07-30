{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Boolean where

import qualified Data.Bool as Bool
import qualified Data.ByteString.Builder as Builder
import qualified Text.Parsec as Parsec

newtype Boolean = MkBoolean
  { unwrap :: Bool
  }
  deriving (Eq, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Boolean
decode =
  MkBoolean
    <$> Parsec.choice
      [ False <$ Parsec.string' "false",
        True <$ Parsec.string' "true"
      ]

encode :: Boolean -> Builder.Builder
encode = Builder.stringUtf8 . Bool.bool "false" "true" . unwrap
