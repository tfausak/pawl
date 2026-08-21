module Pawl.Types.RequireAttack where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Effect's RequireAttack arm: CR 508.1d's
-- requirement that the creatures named attack the players named, for this
-- duration. Alluring Siren's "target creature an opponent controls attacks you
-- this turn if able".
--
-- The two sides are DIFFERENT types where Pawl.Types.RequireBlock's are both
-- ObjectRefs, and that is CR 508.1b: what a creature attacks is a player, a
-- planeswalker or a battle, and the requirement's object is the PLAYER arm
-- (Pawl.Types.ActiveAttackRequirement argues why it is only that arm). Named
-- fields all the same, so a card file reads as the sentence it transcribes.
data RequireAttack = MkRequireAttack
  { duration :: Duration.Duration,
    attacker :: ObjectRef.ObjectRef,
    defender :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
