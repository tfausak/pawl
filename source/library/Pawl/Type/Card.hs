module Pawl.Type.Card where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Pawl.Type.Effect (Effect)
import Pawl.Type.Keyword (Keyword)
import Pawl.Type.ManaCost (ManaCost)
import Pawl.Type.Power (Power)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.StaticAbility (StaticAbility)
import Pawl.Type.TargetSpec (TargetSpec)
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
    keywords :: Set Keyword,
    -- CR 604.1/604.2: this card's static continuous abilities (Humility). Empty
    -- for everything but the few printings that generate a continuous effect just
    -- by being on the battlefield. The projection gathers these live.
    staticAbilities :: [StaticAbility],
    -- The card's rules text as data. Ordered: CR 608.2c resolves instructions
    -- in the order written.
    effects :: [Effect],
    -- The target slots effects reference by name. A Map, not a parallel list:
    -- slots have no order, only names, and nothing aligns by position.
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
