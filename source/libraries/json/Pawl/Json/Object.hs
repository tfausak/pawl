{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Object where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Extra.Monoid as Monoid
import qualified Pawl.Extra.Semigroup as Semigroup
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Pair as Pair
import qualified Text.Parsec as Parsec

newtype Object a = MkObject
  { unwrap :: [Pair.Pair a]
  }
  deriving (Eq, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Object a)
decode p =
  fmap MkObject
    . Parsec.between (Parsec.char '{' <* Parsec.many Array.decodeBlank) (Parsec.char '}')
    $ Parsec.sepBy (Pair.decode p <* Parsec.many Array.decodeBlank) (Parsec.char ',' <* Parsec.many Array.decodeBlank)

encode :: (a -> Builder.Builder) -> Object a -> Builder.Builder
encode b =
  Semigroup.around (Builder.charUtf8 '{') (Builder.charUtf8 '}')
    . Monoid.sepBy (Builder.charUtf8 ',')
    . fmap (Pair.encode b)
    . unwrap
