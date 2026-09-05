module Pawl.Types.RequiredDefender where

-- | CR 508.1d: WHOM a printed attacking requirement says the creature has to
-- attack, named by relation to the permanent printing it. Public Enemy's "all
-- creatures attack enchanted creature's controller each combat if able".
--
-- Pawl.Types.ActiveAttackRequirement, the resolution-created sibling, states the
-- same axis as a bare PlayerId, because its producer TARGETS the player (CR
-- 115.1). A static ability has no target and no slot, so neither
-- Pawl.Types.PlayerRef -- whose ControllerOf arm names a slot -- nor
-- Pawl.Types.PlayerScope -- resolved against CR 109.5's "you", which is the
-- Aura's controller and not the enchanted creature's -- can say this.
--
-- An arm may name SEVERAL players, which is CR 508.1d's own reading: the rule
-- asks whether a declaration attacks a player the requirement names, so a
-- requirement naming a tied-for-the-lead pair is obeyed by attacking either.
-- Pawl.Engine.AttackRequirement's `admissible` is where that becomes a filter
-- over CR 508.1b's announcements rather than a single seat.
--
-- Two arms, and the sweep that says which printings would want a third:
-- Scryfall `o:/attacks [a-z ]{2,40} if able/ game:paper` (2026-09-05), read
-- clause by clause. Every other printing narrowing the object either creates
-- its requirement by RESOLUTION -- Ruhan of the Fomori, Raving Dead, Nahiri the
-- Unforgiving, Dulcet Sirens -- which is ActiveAttackRequirement's family, or
-- prints the narrowing as a CR 508.1c RESTRICTION beside an unnarrowed
-- requirement -- Xantcha, Sleeper Agent, Fealty to the Realm -- which
-- Pawl.Types.CombatRestriction carries. Two would want an arm here: Cogwork
-- Tracker's "a player you noted for cards named Cogwork Tracker", a draft
-- designation with no carrier in pawl at all, and Trove of Temptation's "you or
-- a planeswalker you control", whose subject clause is unwritable anyway
-- (#3257).
data RequiredDefender
  = -- | CR 108.4 / 303.4m: the controller of the object the source is attached
    -- to. Names nobody when the source is attached to nothing, or to a player
    -- (Pawl.Engine.Projection.View.hostOf answers Nothing for both).
    ControllerOfAttached
  | -- | CR 109.5 / 102.3: an opponent of the source's controller holding the
    -- greatest life total among them (Galactus, Devourer of Worlds). Names every
    -- opponent tied for the lead, and nobody when there is no opponent left.
    OpponentWithMostLife
  deriving (Bounded, Enum, Eq, Ord, Show)
