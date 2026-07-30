module Pawl.Exceptions.UnknownCard where

import qualified Control.Exception as Exception
import qualified Pawl.Slug as Slug

-- | The pool holds no file for this slug.
data UnknownCard = MkUnknownCard
  { slug :: Slug.Slug,
    path :: FilePath
  }
  deriving (Eq, Show)

instance Exception.Exception UnknownCard
