module Pawl.Types.TypeLine where

import Data.Set (Set)
import Pawl.Types.CardType (CardType)
import Pawl.Types.Subtype (Subtype)
import Pawl.Types.Supertype (Supertype)

data TypeLine = MkTypeLine
  { supertypes :: Set Supertype,
    types :: Set CardType,
    subtypes :: Set Subtype
  }
  deriving (Eq, Ord, Show)
