module Pawl.Types.CastFrom where

import qualified Pawl.Types.InZone as InZone
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The payload of Pawl.Types.Quantity's WasCastFrom arm: WHO cast the spell (CR
-- 601.2a) and WHICH zone it was cast out of (CR 400.1).
--
-- TWO references and not three. The rule distinguishes three roles -- the
-- caster, the owner of the zone's copy and the owner of the card -- but CR 400.3
-- puts a card only in its OWNER's library, hand or graveyard, so for every
-- per-player zone the last two are one question, and CR 400.1's shared zones
-- have no per-player copy for either to name. The printed clauses constrain the
-- two halves independently and in opposite directions: Breathless Knight's "you
-- cast it from a graveyard" is @Relative You@ over @EachPlayer@'s graveyards,
-- Fblthp, the Lost's "was cast from your library" is @EachPlayer@ over your own.
data CastFrom = MkCastFrom
  { caster :: PlayerRef.PlayerRef,
    from :: InZone.InZone
  }
  deriving (Eq, Ord, Show)
