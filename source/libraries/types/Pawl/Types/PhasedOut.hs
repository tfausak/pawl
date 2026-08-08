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
-- What is deliberately NOT stored is the host an indirect row came in with.
-- Object.attachedTo already names it and phasing does not clear that field (CR
-- 702.26i needs it intact), so a second copy here could only drift out of step
-- with the first.
data PhasedOut
  = Directly {under :: PlayerId.PlayerId}
  | Indirectly {under :: PlayerId.PlayerId}
  deriving (Eq, Ord, Show)
