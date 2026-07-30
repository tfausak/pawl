{-# LANGUAGE FlexibleContexts #-}

module Pawl.Json.Array where

import qualified Data.ByteString.Builder as Builder
import qualified Pawl.Extra.Monoid as Monoid
import qualified Pawl.Extra.Semigroup as Semigroup
import qualified Text.Parsec as Parsec

newtype Array a = MkArray
  { unwrap :: [a]
  }
  deriving (Eq, Show)

decode :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m a -> Parsec.ParsecT s u m (Array a)
decode p =
  fmap MkArray
    . Parsec.between (Parsec.char '[' <* Parsec.many blank) (Parsec.char ']')
    $ Parsec.sepBy (p <* Parsec.many blank) (Parsec.char ',' <* Parsec.many blank)

blank :: (Parsec.Stream s m Char) => Parsec.ParsecT s u m Char
blank = Parsec.oneOf " \t\n\r"

encode :: (a -> Builder.Builder) -> Array a -> Builder.Builder
encode b =
  Semigroup.around (Builder.charUtf8 '[') (Builder.charUtf8 ']')
    . Monoid.sepBy (Builder.charUtf8 ',')
    . fmap b
    . unwrap
