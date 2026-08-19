module Pawl.Types.SpellWasCast where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics
import qualified Pawl.Types.Zone as Zone

-- | CR 601.2i: a spell became cast -- who cast it, which object it is on the
-- stack, that object's characteristics as of the cast, and the zone CR 601.2a
-- moved it out of.

-- Named for the event rather than for the constructor, since Pawl.Types.SpellCast
-- is already the TRIGGER CONDITION's payload. The two describe the same rule from
-- opposite ends: that one says which casts an ability watches for, this one says
-- a cast happened.
--
-- The characteristics are what a LOOK-BACK reader needs (CR 608.2i's "for each
-- spell you've cast this turn"): by the time the counting ability resolves, the
-- spell's stack incarnation is gone.
data SpellWasCast = MkSpellWasCast
  { player :: PlayerId.PlayerId,
    spell :: ObjectId.ObjectId,
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics,
    -- | CR 601.2a's "moves that card ... from where it is to the stack": where
    -- it was. Recorded on the event for the reason the characteristics above are
    -- -- CR 400.7 makes the stack incarnation a new object with no memory of it,
    -- so a reader that came later could not derive it -- and it is the whole of
    -- what Pawl.Types.SpellCast.zone matches against.
    --
    -- Nothing when the cast was proposed for a card no longer findable at that
    -- point, which is Pawl.Engine.Cast's own `castFrom`: the value is read off
    -- the object BEFORE the move, so it is Just for every cast that completes.
    zone :: Maybe Zone.Zone
  }
  deriving (Eq, Ord, Show)
