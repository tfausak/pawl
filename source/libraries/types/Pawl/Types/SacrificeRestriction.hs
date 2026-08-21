module Pawl.Types.SacrificeRestriction where

import qualified Pawl.Types.Affected as Affected

-- | CR 701.21a / CR 101.2: one printed SACRIFICE PROHIBITION -- an effect saying
-- a permanent "can't be sacrificed". Garland, Royal Kidnapper's third clause
-- ("creatures you control but don't own get +2/+2 and can't be sacrificed") is
-- the pool's printing.
--
-- The NINTH carrier of a printed static ability, alongside
-- Pawl.Types.StaticAbility, Pawl.Types.PlayerStaticAbility,
-- Pawl.Types.BlockRequirement, Pawl.Types.AttackRequirement,
-- Pawl.Types.CombatRestriction, Pawl.Types.AttackCost, Pawl.Types.BlockCost and
-- Pawl.Types.BlockPermission. The FIRST of them
-- that is not about combat, which is why its reader is not a combat module:
-- Pawl.Types.BlockRequirement's header argues why neither of the first two can
-- hold one of these, and every step of that argument holds here unchanged.
--
-- NOT a Pawl.Types.Modification, and that is a
-- rules distinction rather than a filing convenience: every arm of that type is
-- a CHARACTERISTIC change computed inside CR 613's layers, where CR 613.11 puts
-- this in the class of continuous effects that "affect game rules rather than
-- objects", and CR 101.2a says outright that such an effect is not an ability
-- being added or removed. Pawl.Engine.Projection sees none of these.
--
-- CR 101.2 is what gives the prohibition its force: "if a rule or effect allows
-- or directs something to happen, and another effect states that it can't
-- happen, the 'can't' effect takes precedence". So it beats a cost that calls
-- for a sacrifice (CR 118.3 then makes that cost unpayable), an edict, and CR
-- 704's own rule-mandated sacrifices alike.
--
-- ONE field rather than a sum, where Pawl.Types.CombatRestriction has five
-- arms: those exist because CR 508.1c and CR 509.1b name two declarations and
-- three shapes to forbid, and CR 701.21a's sacrifice is one game action with
-- nothing to tell apart. A prohibition names a SUBJECT and nothing else.
--
-- NO "unless" gate beside the subject, where every CombatRestriction arm
-- carries one: CR 508.1c writes the clause into the rule, and CR 701.21a has no
-- counterpart -- so a gate here would be a field no rule and no card asks for.
-- The pool's conditional prohibitions (Zurgo, Thunder's Decree; Stilt-Man,
-- Towering Terror) print theirs as a GRANTED ability with its own duration
-- rather than as a clause on the sentence, which is a different shape again.
--
-- Gathered LIVE from the battlefield on every read and never captured, the
-- posture all six siblings take -- so a Garland leaving the battlefield lifts
-- its prohibition with nothing to unwind, and CR 101.2's "can't" is re-asked at
-- the moment each sacrifice is attempted.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.SacrificeRestriction is the only module that may read it, and it
-- answers a set of ids.
newtype SacrificeRestriction = MkSacrificeRestriction
  { -- | Which permanents can't be sacrificed. An Affected, not a bare ObjectId,
    -- so the set is re-derived every time it is asked -- CR 613.11 lets a
    -- rule-modifying continuous effect reach objects that were not affected
    -- when it began, which is exactly what Garland's set does as control of a
    -- creature changes. The field name Pawl.Types.CombatRestriction's arms
    -- spell "affected", and for its reason: this names the restricted
    -- permanents, never an object they act on.
    affected :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
