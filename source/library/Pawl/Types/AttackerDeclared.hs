module Pawl.Types.AttackerDeclared where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 508.1: a creature was declared attacking, whom it is attacking, and how
-- many creatures that declaration named.

-- The count is carried rather than counted out of the log, because CR 702.83b
-- scopes "alone" to a given combat phase while the log is cleared per TURN: a
-- creature in the second combat phase of an extra-combat turn would otherwise not
-- be alone.
data AttackerDeclared = MkAttackerDeclared
  { attacker :: ObjectId.ObjectId,
    defender :: PlayerId.PlayerId,
    count :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
