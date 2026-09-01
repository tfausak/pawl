module Pawl.Types.CantAttackPlayer where

import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 508.1c's PAIRWISE restriction: which creatures are restricted, which
-- players they may not attack, and CR 508.1c's "unless" gate. Blazing Archon's
-- "creatures can't attack you".

-- Its own record rather than Pawl.Types.AffectedUnless plus a field, for
-- Pawl.Types.CantBeBlockedBy's reason: 'affected' names the creatures restricted
-- and 'defenders' names the players they may not be announced against, so the
-- two halves are not interchangeable and the second key is not a second
-- @affected@.
--
-- A Pawl.Types.PlayerScope and not a Pawl.Types.Filter over players, because CR
-- 109.5 is the whole of what the printed sentence needs: "you" is the source's
-- controller. Pawl.Types.RequiredDefender says why the REQUIREMENT side could
-- not take this type -- Public Enemy's object is the enchanted creature's
-- controller, which is not CR 109.5's "you" -- and that asymmetry is why the two
-- axes carry different types rather than sharing one.
--
-- Players and not attack targets: CR 506.3 lets a player, a planeswalker or a
-- battle be attacked, and the printed sentence names only the first. A
-- planeswalker its controller is protected from being attacked through is a
-- different announcement (CR 508.1b), which Blazing Archon does not forbid.
-- Not implemented: a restriction naming a planeswalker or a battle, "can't
-- attack you or planeswalkers you control" (#2891).
data CantAttackPlayer = MkCantAttackPlayer
  { affected :: Affected.Affected,
    defenders :: PlayerScope.PlayerScope,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
