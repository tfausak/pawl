module Pawl.Type.UnslugifiableFile where

import qualified Control.Exception as Exception

-- A .json file in the pool whose own file name is not a slug. Raised by
-- Pawl.Registry.slugs: a lookup builds its path from a slug, so such a file can
-- never be opened by name -- enumerating past it would report a pool larger than
-- the one anything can actually load. Distinct from MisfiledCard, which is about
-- a file whose CONTENTS disagree with its (valid) name.
newtype UnslugifiableFile = MkUnslugifiableFile FilePath
  deriving (Eq, Show)

instance Exception.Exception UnslugifiableFile
