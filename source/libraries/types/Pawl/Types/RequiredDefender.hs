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
-- ONE arm, because one printing spells the object this way. Not implemented:
-- the other spellings, which want a set of players rather than a relation to one
-- object (#2820).
data RequiredDefender
  = -- | CR 108.4 / 303.4m: the controller of the object the source is attached
    -- to. Names nobody when the source is attached to nothing, or to a player
    -- (Pawl.Engine.Projection.hostOf answers Nothing for both).
    ControllerOfAttached
  deriving (Bounded, Enum, Eq, Ord, Show)
