{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Value where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Boolean as Boolean
import qualified Pawl.Json.Null as Null
import qualified Pawl.Json.Number as Number
import qualified Pawl.Json.Object as Object
import qualified Pawl.Json.String as String
import qualified Text.Parsec as Parsec

data Value
  = Null Null.Null
  | Boolean Boolean.Boolean
  | Number Number.Number
  | String String.String
  | Array (Array.Array Value)
  | Object (Object.Object Value)
  deriving (Eq, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Value
decode =
  Parsec.between (Parsec.many Array.decodeBlank) (Parsec.many Array.decodeBlank) $
    Parsec.choice
      [ Null <$> Null.decode,
        Boolean <$> Boolean.decode,
        Number <$> Number.decode,
        String <$> String.decode,
        Array <$> Array.decode decode,
        Object <$> Object.decode decode
      ]

encode :: Value -> Builder.Builder
encode v = case v of
  Null n -> Null.encode n
  Boolean b -> Boolean.encode b
  Number n -> Number.encode n
  String s -> String.encode s
  Array a -> Array.encode encode a
  Object o -> Object.encode encode o
