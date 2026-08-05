module Pawl.Exceptions.InvalidCorpus where

import qualified Control.Exception as Exception

-- | A registry was pointed at a root it could not use: files that would not
-- parse, or one name claimed by more than one card.
--
-- Every problem at once rather than the first, so a broken pool is one report
-- rather than N runs (#167).
--
-- Rendered `problems` rather than a structured list, because this sublibrary
-- sits above `types` and cannot speak CardName, and because a caller's only
-- move is to show them.
data InvalidCorpus = MkInvalidCorpus
  { root :: FilePath,
    problems :: [String]
  }
  deriving (Eq, Show)

instance Exception.Exception InvalidCorpus
