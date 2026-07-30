module Pawl.Extra.Parsec where

import qualified Pawl.Extra.Either as Either
import qualified Text.Parsec as Parsec

-- | Attempts to parse the string using the given parser.
parseString :: Parsec.Parsec String () a -> String -> Maybe a
parseString p = Either.hush . Parsec.parse p ""
