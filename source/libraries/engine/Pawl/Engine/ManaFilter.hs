-- CR 106.1: the one place a Pawl.Types.ManaFilter is interpreted. A predicate
-- over one unit of mana, the mana-side counterpart of
-- Pawl.Engine.Filter.matches -- and, like it, identity-blind: it never learns
-- which effect or card asked.
module Pawl.Engine.ManaFilter where

import qualified Pawl.Types.ManaFilter as ManaFilter
import qualified Pawl.Types.ManaType as ManaType
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.ManaUnit as ManaUnit

-- | CR 106.1b: a unit's type is the whole of what a filter can ask today, so
-- this is a comparison and not a fold -- see Pawl.Types.ManaFilter for why the
-- production tags are not reachable from here.
matchesType :: ManaFilter.ManaFilter -> ManaType.ManaType -> Bool
matchesType filter_ manaType = case filter_ of
  ManaFilter.Any -> True
  ManaFilter.OfType wanted -> manaType == wanted
  ManaFilter.NotOfType unwanted -> manaType /= unwanted

-- | The same question about a unit in a pool. Kept beside the type-level one
-- because a SUPPLY (Pawl.Engine.Mana.Supply) may not have settled on a type
-- yet, so the filter has to be askable of a bare type as well.
matches :: ManaFilter.ManaFilter -> ManaUnit -> Bool
matches filter_ unit = matchesType filter_ (ManaUnit.manaType unit)
