module Pawl.Type.MissingRoot where

import qualified Control.Exception as Exception

-- A registry was pointed at a directory that does not exist. Raised by
-- Pawl.Registry.new rather than by the first lookup, so a mistyped --cards-dir
-- fails once, at startup, instead of once per card looked up.
newtype MissingRoot = MkMissingRoot FilePath
  deriving (Eq, Show)

instance Exception.Exception MissingRoot
