module Pawl.Types.CrewRestriction where

import qualified Pawl.Types.Affected as Affected

-- | CR 702.122d / CR 101.2: one printed CREW PROHIBITION -- an effect saying a
-- creature "can't crew Vehicles". Revoke Privileges' third clause is the pool's
-- printing; Bound in Gold and Intercessor's Arrest print the same one.
--
-- Pawl.Types.SacrificeRestriction's shape and its filing, one game action over:
-- CR 613.11 puts a continuous effect that "affects game rules rather than
-- objects" outside the layer system, CR 101.2a says such an effect is not an
-- ability being added or removed, and no layer of Pawl.Engine.Projection
-- rewrites them. Every step of that type's argument for why it cannot be a
-- Pawl.Types.Modification holds here unchanged.
--
-- CR 101.2 is what gives the prohibition its force over CR 702.122a's cost: the
-- keyword's ability directs a tap and this states it can't happen, so the
-- "can't" wins and CR 118.3 then makes that cost unpayable by the creature.
--
-- ONE field rather than a sum, for SacrificeRestriction's reason: rule 702.122d
-- names one thing a creature may not be tapped for, and a prohibition names a
-- subject and nothing else. No "unless" gate beside it either -- the pool's
-- printings state none, and CR 702.122d writes no clause into the rule the way
-- CR 508.1c does.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture every sibling carrier takes: a Revoke Privileges that left lifts its
-- prohibition with nothing to unwind.
--
-- THE CREW COST ALONE, which is what rule 702.122d names. The same
-- Pawl.Types.CostComponent that pays a crew ability's cost is printable outside
-- one (data/cards/synthetic-crewed-battery.json), so the narrowing is made where
-- the KEYWORD is known: Pawl.Engine.Keyword's `crew` is the only minter of
-- Pawl.Types.Filter's CantCrewVehicles atom, and Pawl.Engine.Cost.tapCandidates
-- is what supplies the set the atom reads.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.CrewRestriction is the only module that may read it, and it
-- answers a set of ids.
newtype CrewRestriction = MkCrewRestriction
  { -- | Which creatures can't crew Vehicles. An Affected, not a bare ObjectId,
    -- so the set is re-derived every time it is asked -- CR 613.11 lets a
    -- rule-modifying continuous effect reach objects that were not affected when
    -- it began, which is what Revoke Privileges' set does as the Aura moves.
    affected :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
