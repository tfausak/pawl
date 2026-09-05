module Pawl.Types.AttackRequirement where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.RequiredDefender as RequiredDefender

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
    -- One requirement per creature the Affected matches, which is CR 508.1d's
    -- own counting. Not implemented: a requirement over a GROUP, obeyed once by
    -- any member -- Seeker of Slaanesh's "each opponent must attack with at
    -- least one creature each combat if able" (#3257).
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
    -- Pawl.Types.BlockRequirement is the twin that does not carry this yet: CR
    -- 509.1c words the shape the same way, and nothing in data/cards/ prints a
    -- conditional BLOCKING requirement -- Enkira, Hostile Scavenger and Frodo
    -- Baggins are printings that would, untranscribed. Not implemented: that
    -- half (#2036).
    while :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
