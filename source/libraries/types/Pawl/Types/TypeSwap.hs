module Pawl.Types.TypeSwap where

import qualified Pawl.Types.Subtype as Subtype

-- | CR 612.1: the word a text-changing effect replaces, and its replacement.
data TypeSwap = MkTypeSwap
  { from :: Subtype.Subtype,
    to :: Subtype.Subtype
  }
  deriving (Eq, Ord, Show)
