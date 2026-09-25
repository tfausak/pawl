module Pawl.Types.AttackerDeclared where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 508.1: a creature was declared attacking, whom it is attacking, what it
-- was announced at, how many creatures that declaration named, and the attacking
-- player who declared it.

-- The count is carried rather than counted out of the log, because CR 702.83b
-- scopes "alone" to a given combat phase while the log is cleared per TURN: a
-- creature in the second combat phase of an extra-combat turn would otherwise not
-- be alone.
--
-- `defender` and `target` are both here because CR 508.3a's two sentences ask
-- different questions of one declaration: the first resolves through CR 508.5's
-- defending player, and the second through CR 508.1b's announcement, which is a
-- player, a planeswalker or a battle. Neither derives from the other here -- a
-- battle's protector (CR 310.9d) is a seat the target does not name, and CR
-- 506.4c can take the announced permanent out of combat while the creature stays
-- in it.
--
-- `attackingPlayer` is the creature's controller as it was declared. The active
-- player in every game but one using the shared team turns option, where each
-- player on the active team is an attacking player (CR 805.10a) and "you
-- attacked" asks about one of them (CR 805.10c).
data AttackerDeclared = MkAttackerDeclared
  { attacker :: ObjectId.ObjectId,
    defender :: PlayerId.PlayerId,
    target :: AttackTarget.AttackTarget,
    count :: Natural.Natural,
    attackingPlayer :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
