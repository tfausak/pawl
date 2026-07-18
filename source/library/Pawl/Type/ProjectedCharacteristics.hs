module Pawl.Type.ProjectedCharacteristics where

import Data.Set (Set)
import Pawl.Type.Keyword (Keyword)

-- The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. No Ord: never sorted, never a key.
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    power :: Maybe Integer,
    toughness :: Maybe Integer
  }
  deriving (Eq, Show)
