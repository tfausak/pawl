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
--
-- An arm is answered on one of two roads, and which one is a property of the
-- payload rather than a choice: a payload nothing but the rule asking it can
-- observe is read back off CR 400.7d's record when that rule asks
-- (Pawl.Engine.ManaRider.uncounterable), and one with an independently
-- observable existence is MINTED at payment (Pawl.Engine.ManaRider.granted),
-- which is CR 106.6a's "a separate effect is created once for each mana
-- produced".
data ManaRiderEffect
  = -- | CR 701.6a, denied through CR 101.2: the spell the mana paid for can't
    -- be countered. Boseiju, Who Shelters All's "if that mana is spent on an
    -- instant or sorcery spell, that spell can't be countered" and Delighted
    -- Halfling's "and that spell can't be countered" are the printings.
    --
    -- NOT Pawl.Types.Counterability, which is CR 113.6g -- an object's OWN
    -- ability about itself, a field of its face that nothing writes at runtime.
    -- NOT Pawl.Types.PlayerEffect.CantBeCountered either, which is CR 613.11's
    -- class-scoped rules modification (Prowling Serpopard); its scope is a
    -- Pawl.Types.Filter, which has no atom naming an arbitrary ObjectId, so
    -- that carrier cannot be narrowed to the one spell this mana paid for.
    -- (Pawl.Types.Affected's TheseObjects, which the arm below stores, is a
    -- different axis and names one object exactly.)
    CantBeCountered
  | -- | CR 702.10 for a duration CR 514.2 ends: the spell the mana paid for
    -- gains haste until end of turn. Generator Servant's "if that mana is spent
    -- on a creature spell, it gains haste until end of turn" is the printing.
    --
    -- The grant rides CR 400.7a onto the permanent the spell becomes
    -- (Pawl.Engine.Event.carryOver), which is what makes it observable at all --
    -- a haste on a spell is nothing.
    GainsHasteUntilEndOfTurn
  deriving (Bounded, Enum, Eq, Ord, Show)
