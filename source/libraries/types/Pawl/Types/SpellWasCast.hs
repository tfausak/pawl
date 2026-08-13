module Pawl.Types.SpellWasCast where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as ProjectedCharacteristics

-- | CR 601.2i: a spell became cast -- who cast it, which object it is on the
-- stack, and that object's characteristics as of the cast.

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
    characteristics :: ProjectedCharacteristics.ProjectedCharacteristics
  }
  deriving (Eq, Ord, Show)
