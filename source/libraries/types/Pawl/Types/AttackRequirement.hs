module Pawl.Types.AttackRequirement where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.RequiredDefender as RequiredDefender
import qualified Pawl.Types.RequirementArity as RequirementArity

-- | CR 508.1d: one printed ATTACKING REQUIREMENT. Curse of the Nightly Hunt's
-- "creatures enchanted player controls attack each combat if able".
--
-- The FOURTH carrier of a printed static ability, and the twin of
-- Pawl.Types.BlockRequirement, which argues why neither
-- Pawl.Types.StaticAbility nor Pawl.Types.PlayerStaticAbility can hold a
-- requirement. BlockRequirement carries BOTH of CR 509.1c's axes, because the
-- printings use both; this one carries all three of subject, object and CR
-- 508.1d's condition.
--
-- Gathered LIVE from the battlefield on every read and never captured, so a
-- Curse leaving the battlefield lifts its requirement with nothing to unwind.
data AttackRequirement = MkAttackRequirement
  { -- | Which creatures are required to attack. An Affected, not a bare
    -- ObjectId, so the set is re-derived every time it is asked. CR 611.2c and
    -- CR 613.11 are why Affected.TheseObjects is the arm this field has no use
    -- for: a rule-modifying continuous effect can affect objects that were not
    -- affected when it began.
    --
    -- How many requirements the Affected states is the `arity` field below,
    -- CR 508.1d's own counting being one per creature.
    subject :: Affected.Affected,
    -- | CR 508.1d's OBJECT axis -- WHOM the creature has to attack. Nothing is
    -- the unnarrowed requirement (Curse of the Nightly Hunt's "attack each
    -- combat if able"), which Pawl.Engine.AttackRequirement instantiates as one
    -- pair per announcement CR 508.1b admits, so any announcement obeys it.
    --
    -- A Pawl.Types.RequiredDefender and not the PlayerId
    -- Pawl.Types.ActiveAttackRequirement carries: that carrier's producer names
    -- its player by TARGETING it, and a static ability has no target to read.
    -- The type says why neither PlayerRef nor PlayerScope reaches the phrase.
    object :: Maybe RequiredDefender.RequiredDefender,
    -- | CR 508.1d's second shape -- "or that it attacks if some condition is
    -- met" -- read as CR 604.2's "as long as" clause. Otarian Juggernaut's
    -- threshold: "as long as there are seven or more cards in your graveyard,
    -- this creature ... attacks each combat if able". Nothing is the ungated
    -- requirement (Curse of the Nightly Hunt, Berserkers of Blood Ridge).
    --
    -- The same field Pawl.Types.BlockPermission spells `while`, with the same
    -- polarity -- a gate that HOLDS is what puts the requirement in force -- and
    -- the opposite of Pawl.Types.CombatRestriction's "unless", where a gate that
    -- holds LIFTS the restriction. Re-read on every look like every other field
    -- here, so a graveyard shrinking below seven lifts the requirement at once,
    -- and the "you" inside it is CR 109.5's: the controller of the permanent
    -- printing the sentence.
    --
    -- Pawl.Types.BlockRequirement spells the twin of this field for CR 509.1c's
    -- identically worded shape, and Seton's Desire is the same threshold clause
    -- on that side.
    while :: Maybe Condition.Condition,
    -- | CR 508.1d counted: whether the sentence states one requirement per
    -- creature the subject names (Curse of the Nightly Hunt) or ONE over them
    -- all, obeyed by any single one of them (Seeker of Slaanesh's "each
    -- opponent must attack with at least one creature each combat if able").
    --
    -- ONE group per requirement, where Pawl.Types.BlockRequirement's is one per
    -- attacker the object axis names: this side's object axis says WHOM the
    -- creature must attack rather than naming a second set of creatures, so the
    -- group is the subject set crossed with every announcement the object
    -- admits, obeyed by attacking with any one of them.
    arity :: RequirementArity.RequirementArity
  }
  deriving (Eq, Ord, Show)
