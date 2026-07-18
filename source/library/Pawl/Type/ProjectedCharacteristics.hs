module Pawl.Type.ProjectedCharacteristics where

import Data.Set (Set)
import Pawl.Type.CardType (CardType)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.Subtype (Subtype)

-- The characteristics of an object after the layer fold (design.md §2.5). Maybe
-- P/T because a land has none. cardTypes/subtypes are the projected type line
-- (CR 613 layer 4). rulesTextActive is CR 305.7: False once an effect SETS this
-- object's land subtype to a basic type, stripping its rules-text abilities.
-- No Ord: never sorted, never a key.
data ProjectedCharacteristics = MkProjectedCharacteristics
  { keywords :: Set Keyword,
    power :: Maybe Integer,
    toughness :: Maybe Integer,
    cardTypes :: Set CardType,
    subtypes :: Set Subtype,
    rulesTextActive :: Bool
  }
  deriving (Eq, Show)
