module Pawl.Types.IncreaseActivationCost where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's IncreaseActivationCost arm, the
-- ACTIVATION half of what IncreaseSpellCost says about a spell (CR 601.2f
-- through CR 602.2b).
--
-- The amount is GENERIC mana (CR 601.2f), IncreaseSpellCost's reason one
-- constructor over: no printing taxes an activation by a coloured symbol.
--
-- ONE criterion where ReduceActivationCost carries two. `whichAbilities` names
-- the ability's SOURCE OBJECT -- Oppressive Rays' "activated abilities of
-- enchanted creature" -- and is matched by
-- Pawl.Engine.PlayerEffect.matchesObjectFrom against that object's projection.
-- There is no `grantedBy` beside it, because no producer narrows an increase by
-- the KIND of ability. Not implemented: CR 605.1a's "unless they're mana
-- abilities", which Suppression Field prints (#2293).
data IncreaseActivationCost = MkIncreaseActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
