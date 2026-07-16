module Pawl.Type.Card where

import Data.Text (Text)
import Pawl.Type.ManaCost (ManaCost)
import Pawl.Type.Power (Power)
import Pawl.Type.Toughness (Toughness)
import Pawl.Type.TypeLine (TypeLine)

data Card = MkCard
  { name :: Text,
    -- Nothing, not a zero cost: CR 202.1, a land has no mana cost at all.
    manaCost :: Maybe ManaCost,
    typeLine :: TypeLine,
    -- Only creatures have these.
    power :: Maybe Power,
    toughness :: Maybe Toughness
  }
  deriving (Eq, Ord, Show)
