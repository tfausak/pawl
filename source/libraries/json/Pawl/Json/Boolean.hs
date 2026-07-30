{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Boolean where

import qualified Data.ByteString.Builder as Builder
import qualified Text.Parsec as Parsec

newtype Boolean
  = MkBoolean Bool
  deriving (Eq, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Boolean
decode =
  MkBoolean
    <$> Parsec.choice
      [ False <$ Parsec.string' "false",
        True <$ Parsec.string' "true"
      ]

encode :: Boolean -> Builder.Builder
encode (MkBoolean b) = Builder.stringUtf8 $ if b then "true" else "false"
