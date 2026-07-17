module Pawl.Type.Card where

import Data.Set (Set)
import Data.Text (Text)
import Pawl.Type.Keyword (Keyword)
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
    toughness :: Maybe Toughness,
    -- CR 702. A Set because CR 702.9c and 702.3c say multiple instances are
    -- redundant -- a per-keyword fact, true of everything through M2c, and NOT
    -- true out in the tail (two Wards both trigger; Rampage stacks).
    --
    -- The closed half must read this through Pawl.Game.keywordsOf, never
    -- directly: layer 6 grants and removes abilities at M3.
    keywords :: Set Keyword
  }
  deriving (Eq, Ord, Show)
