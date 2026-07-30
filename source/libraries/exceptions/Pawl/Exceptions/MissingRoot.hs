module Pawl.Exceptions.MissingRoot where

import qualified Control.Exception as Exception

-- | A registry was pointed at a directory that does not exist.
newtype MissingRoot = MkMissingRoot
  { path :: FilePath
  }
  deriving (Eq, Show)

instance Exception.Exception MissingRoot
