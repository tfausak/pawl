module Pawl.Types.PhasedOut where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 702.26b's status, plus the one distinction CR 702.26g draws between the
-- two ways of reaching it. Every phased-out permanent carries the player who
-- controlled it WHEN IT PHASED OUT (CR 702.26a's comparand, which CR 702.26e
-- takes the live answer away from), and says which of rule 702.26's two
-- schedules it is on.
--
-- `Directly` is CR 702.26a's own schedule: the permanent phased out because it
-- had phasing during that player's untap step, and it phases in during that
-- player's next untap step. `Indirectly` is rule 702.26g's -- "an Aura,
-- Equipment, or Fortification that phased out indirectly won't phase in by
-- itself, but instead phases in along with the permanent it's attached to" --
-- so the stored player is NOT a schedule for it, only the answer to who
-- controlled it. CR 702.26h is why one object cannot be both: an object that
-- would phase out both ways "just phases out indirectly".
--
-- `Orphaned` is CR 702.26n's second sentence, and a direct row in every other
-- respect: the player it phased out under has since left the game, so CR 800.4k
-- gives them no further untap step and rule 702.26a's schedule can never come
-- round. Rule 702.26n reschedules it to "the next untap step after that player's
-- next turn would have begun", which is the untap step of whichever seat the
-- turn order reaches instead. The stored player is kept rather than re-keyed to
-- that seat, because CR 702.26a defines it as who controlled the permanent when
-- it phased out and Pawl.Engine.Phasing.phasedOutUnder answers that question.
--
-- What is deliberately NOT stored is the host an indirect row came in with.
-- Object.attachedTo already names it and phasing does not clear that field (CR
-- 702.26i needs it intact), so a second copy here could only drift out of step
-- with the first.
data PhasedOut
  = Directly {under :: PlayerId.PlayerId}
  | Indirectly {under :: PlayerId.PlayerId}
  | Orphaned {under :: PlayerId.PlayerId}
  deriving (Eq, Ord, Show)
