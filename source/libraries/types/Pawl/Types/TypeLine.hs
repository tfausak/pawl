module Pawl.Types.TypeLine where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

data TypeLine = MkTypeLine
  { supertypes :: Set.Set Supertype.Supertype,
    types :: Set.Set CardType.CardType,
    subtypes :: Set.Set Subtype.Subtype
  }
  deriving (Eq, Ord, Show)
