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
-- Two arms, because two printings spell the object. The third spelling in the
-- pool is Trove of Temptation's "you or a planeswalker you control", which mixes
-- a seat with CR 508.1b's planeswalker announcements; that card's subject clause
-- is unwritable too, so nothing needs the arm until #3257 lands.
data RequiredDefender
  = -- | CR 108.4 / 303.4m: the controller of the object the source is attached
    -- to. Names nobody when the source is attached to nothing, or to a player
    -- (Pawl.Engine.Projection.View.hostOf answers Nothing for both).
    ControllerOfAttached
  | -- | CR 109.5 / 102.3: an opponent of the source's controller whose life
    -- total is the greatest among that controller's opponents. Galactus,
    -- Devourer of Worlds' "attacks an opponent with the most life among your
    -- opponents each combat if able". Names every opponent tied for the lead,
    -- and nobody when the controller has no opponent left.
    OpponentWithMostLife
  deriving (Bounded, Enum, Eq, Ord, Show)
