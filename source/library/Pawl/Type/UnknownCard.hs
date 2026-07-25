module Pawl.Type.UnknownCard where

import qualified Control.Exception as Exception
import Pawl.Type.Slug (Slug)

-- The pool holds no file for this slug. Carries the path it looked for as well
-- as the slug, since a caller that wants "unknown card X, did you mean...?"
-- needs the slug, and one debugging a wrong --cards-dir needs the path.
data UnknownCard = MkUnknownCard
  { slug :: Slug,
    path :: FilePath
  }
  deriving (Eq, Show)

instance Exception.Exception UnknownCard
