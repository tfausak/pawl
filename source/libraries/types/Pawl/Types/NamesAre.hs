module Pawl.Types.NamesAre where

import qualified Data.Set as Set
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Reference as Reference

-- | CR 201.1: an object's names, all of them; none for a nameless object.
data NamesAre = MkNamesAre
  { object :: Reference.Reference,
    names :: Set.Set CardName.CardName
  }
  deriving (Eq, Ord, Show)
