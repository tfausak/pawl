module Pawl.Types.TypesAre where

import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Reference as Reference

-- | CR 205.2a: an object's card types, all of them.
data TypesAre = MkTypesAre
  { object :: Reference.Reference,
    types :: Set.Set CardType.CardType
  }
  deriving (Eq, Ord, Show)
