-- CR 106.1: the one place a Pawl.Types.ManaFilter is interpreted. A predicate
-- over one unit of mana, the mana-side counterpart of Pawl.Engine.Filter.matches
-- -- and, like it, identity-blind: it never learns which effect or card asked.
module Pawl.Engine.ManaFilter where

import qualified Pawl.Types.ManaFilter as ManaFilter
import Pawl.Types.ManaUnit (ManaUnit)
import qualified Pawl.Types.ManaUnit as ManaUnit

-- CR 106.1b: "there are six types of mana: white, blue, black, red, green, and
-- colorless." A unit's type is the whole of what a filter can ask today, so this
-- is a comparison and not a fold -- see Pawl.Types.ManaFilter for why the
-- production tags are not reachable from here.
matches :: ManaFilter.ManaFilter -> ManaUnit -> Bool
matches filter_ unit = case filter_ of
  ManaFilter.Any -> True
  ManaFilter.OfType manaType -> ManaUnit.manaType unit == manaType
