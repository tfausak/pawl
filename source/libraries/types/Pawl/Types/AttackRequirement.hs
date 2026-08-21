module Pawl.Types.AttackRequirement where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition

-- | CR 508.1d: one printed ATTACKING REQUIREMENT. Curse of the Nightly Hunt's
-- "creatures enchanted player controls attack each combat if able".
--
-- The FOURTH carrier of a printed static ability, and the twin of
-- Pawl.Types.BlockRequirement, which argues why neither
-- Pawl.Types.StaticAbility nor Pawl.Types.PlayerStaticAbility can hold a
-- requirement. BlockRequirement carries BOTH of CR 509.1c's axes, because the
-- printings use both; this one carries the subject and CR 508.1d's condition.
--
-- CR 508.1d's OBJECT axis -- "attacks a player other than you if able" -- lives
-- on Pawl.Types.ActiveAttackRequirement instead, the resolution-created sibling.
-- Nothing in data/cards/ states that axis as a static ability: the one card
-- there that reaches it CREATES the requirement on resolution (Alluring Siren),
-- as does Kardur, Doomscourge, untranscribed. Printings that state it STATICALLY
-- do exist and are untranscribed too, so the gap is a transcription away from
-- being observable: Public Enemy is an Aura reading "all creatures attack
-- enchanted creature's controller each combat if able", and Galactus, Devourer
-- of Worlds prints it on a creature, gated (Scryfall o:"if able" o:"other than
-- you" and the o:"if able" sweep, 2026-08-21). Pawl.Engine.AttackRequirement
-- instantiates this carrier as (creature, target) pairs, one per announcement
-- CR 508.1b admits, so a requirement written here is unrestricted on that axis.
-- Not implemented: a PRINTED requirement that narrows it (#2014).
--
-- Gathered LIVE from the battlefield on every read and never captured, so a
-- Curse leaving the battlefield lifts its requirement with nothing to unwind.
data AttackRequirement = MkAttackRequirement
  { -- | Which creatures are required to attack. An Affected, not a bare
    -- ObjectId, so the set is re-derived every time it is asked. CR 611.2c and
    -- CR 613.11 are why Affected.TheseObjects is the arm this field has no use
    -- for: a rule-modifying continuous effect can affect objects that were not
    -- affected when it began.
    subject :: Affected.Affected,
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
