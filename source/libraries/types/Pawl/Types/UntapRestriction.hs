module Pawl.Types.UntapRestriction where

import qualified Pawl.Types.Affected as Affected

-- | CR 502.1 / CR 101.2: one printed UNTAP PROHIBITION -- an effect saying a
-- permanent "doesn't untap during its controller's untap step". Tsabo's Web's
-- second sentence is the pool's printing.
--
-- Pawl.Types.SacrificeRestriction's shape and its filing, one turn-based action
-- over: CR 613.11 puts a continuous effect that "affects game rules rather than
-- objects" outside the layer system, CR 101.2a says such an effect is not an
-- ability being added or removed, and Pawl.Engine.Projection sees none of them.
-- Every step of that type's argument for why it cannot be a
-- Pawl.Types.Modification holds here unchanged.
--
-- CR 101.2 is what gives the prohibition its force over CR 502.1's untap: the
-- turn-based action directs the untap and this states it can't happen, so the
-- "can't" wins. Nothing else in the rules asks -- an untap the ACTIVE PLAYER's
-- untap step does not perform (Effect.Untap, CR 701.26a) is a different action,
-- which this sentence does not name and so does not touch.
--
-- ONE field rather than a sum, for SacrificeRestriction's reason: CR 502.1 is one
-- action and a prohibition names a subject and nothing else. No "unless" gate
-- beside it either -- the pool's printing states none, and CR 502.1 writes no
-- clause into the rule the way CR 508.1c does.
--
-- Gathered LIVE from the battlefield at the untap step and never captured, the
-- posture every sibling carrier takes: a Tsabo's Web that left lifts its
-- prohibition with nothing to unwind.
--
-- Open-half card data, classified rather than identified:
-- Pawl.Engine.UntapRestriction is the only module that may read it, and it
-- answers a set of ids.
newtype UntapRestriction = MkUntapRestriction
  { -- | Which permanents don't untap. An Affected, not a bare ObjectId, so the set
    -- is re-derived every untap step -- CR 613.11 lets a rule-modifying continuous
    -- effect reach objects that were not affected when it began, which is what
    -- Tsabo's Web's set does as lands enter and leave.
    affected :: Affected.Affected
  }
  deriving (Eq, Ord, Show)
