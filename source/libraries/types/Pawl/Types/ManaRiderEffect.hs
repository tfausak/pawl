module Pawl.Types.ManaRiderEffect where

-- | CR 106.6's second shape, reduced to its payload: WHAT a mana-producing
-- effect does to the spell or ability its mana is spent on, once the rider's
-- condition (Pawl.Types.ManaRider) has matched.
--
-- A CLASSIFICATION and not an effect. Pawl.Types.Effect is the open half's
-- vocabulary and grows forever; this type is the closed half's, and every arm
-- names a question some rule in the rulebook already asks -- which is what lets
-- Pawl.Engine.Event ask it without learning what a card says. Nothing here may
-- ever mean "run these effects": that shape is CR 603.7a's delayed triggered
-- ability, the third of rule 106.6's shapes, and it carries a whole ability
-- rather than a word.
--
-- The interpreter is Pawl.Engine.ManaRider, which is where the casing lives so
-- that Pawl.Engine.Event.counterOne can ask one typed question --
-- Pawl.Engine.PlayerEffect.cantBeCountered's arrangement, and for its reason.
data ManaRiderEffect
  = -- | CR 701.6a, denied through CR 101.2: the spell the mana paid for can't
    -- be countered. Boseiju, Who Shelters All's "if that mana is spent on an
    -- instant or sorcery spell, that spell can't be countered" and Delighted
    -- Halfling's "and that spell can't be countered" are the printings.
    --
    -- NOT Pawl.Types.Counterability, which is CR 113.6g -- an object's OWN
    -- ability about itself, a field of its face that nothing writes at runtime.
    -- NOT Pawl.Types.PlayerEffect.CantBeCountered either, which is CR 613.11's
    -- class-scoped rules modification (Prowling Serpopard); Pawl.Types.Filter
    -- has no atom naming an arbitrary ObjectId, so no such stored effect can be
    -- narrowed to the one spell this mana paid for.
    CantBeCountered
  deriving (Bounded, Enum, Eq, Ord, Show)
