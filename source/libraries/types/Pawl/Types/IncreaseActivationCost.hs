module Pawl.Types.IncreaseActivationCost where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.PlayerEffect's IncreaseActivationCost arm, the
-- ACTIVATION half of what IncreaseSpellCost says about a spell (CR 601.2f
-- through CR 602.2b).
--
-- The amount is GENERIC mana (CR 601.2f), IncreaseSpellCost's reason one
-- constructor over: no printing taxes an activation by a coloured symbol.
--
-- TWO criteria where ReduceActivationCost carries three. `whichAbilities` names
-- the ability's SOURCE OBJECT -- Oppressive Rays' "activated abilities of
-- enchanted creature" -- and is matched by
-- Pawl.Engine.PlayerEffect.matchesObjectFrom against that object's projection.
--
-- `whichKind` is the second, and the source filter cannot say it: CR 605.1a's
-- classification of the ability BEING ACTIVATED, which is Suppression Field's
-- "unless they're mana abilities". Nothing is every activated ability of a
-- matching source, which is what Oppressive Rays prints; Just a kind is only the
-- abilities on that side of the rule. A Filter atom could never stand in --
-- that field is asked of the Mountain, and whether the Mountain's `{T}: Add {R}`
-- is a mana ability is a fact about the ABILITY. Just ManaAbility is
-- expressible and no printing writes it; the field is a classification either
-- way, so admitting the other side costs nothing and asserts nothing.
--
-- There is no `whichTargets` beside them, because no producer narrows an
-- increase by what the ability targets.
data IncreaseActivationCost = MkIncreaseActivationCost
  { whichAbilities :: Filter.Filter Keyword.Keyword,
    whichKind :: Maybe AbilityKind.AbilityKind,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
