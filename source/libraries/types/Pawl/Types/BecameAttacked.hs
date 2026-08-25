module Pawl.Types.BecameAttacked where

import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 508.3b / CR 508.3e: something was attacked, and by whom.

-- Both sides ride one payload because CR 508.3e's "[a player] attacks [another
-- player]" reads them together, and neither of the declaration's other two
-- events can be matched on for it: Pawl.Types.AttackerDeclared names the
-- attacking CREATURE with CR 508.5's defending player and not the target, and
-- GameEvent.AttackersDeclared names the attacking player with no target at all.
--
-- The attacker is CR 508.1's declaring player, the same id
-- GameEvent.AttackersDeclared carries for the same declaration -- read off the
-- event rather than off GameState.activePlayer for that event's reason: the two
-- agree today, rule 508.1 letting only the active player declare, but the rule
-- asks who declared.
data BecameAttacked = MkBecameAttacked
  { attacker :: PlayerId.PlayerId,
    target :: AttackTarget.AttackTarget
  }
  deriving (Eq, Ord, Show)
