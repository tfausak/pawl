module Pawl.Types.BecameUnattached where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 701.3d: one Aura, Equipment or Fortification CEASING to be attached to
-- the object or player it was attached to -- rule 701.3d's "becoming unattached
-- [from that object or player]", which covers the attachment moving elsewhere,
-- the attachment leaving the battlefield, the host leaving its zone and the host
-- player leaving the game. Recorded by Pawl.Engine.Event.unattach, the funnel
-- every route goes through.
--
-- Not recorded by Pawl.Engine.Phasing, which is CR 702.26j: a permanent phasing
-- out keeps its Object.attachedTo, and the rule states outright that becoming
-- unattached does not trigger on phasing. Pawl.PhasingSpec holds that fence.
--
-- BecameAttached's mirror, field for field, and a record for that type's reason:
-- the two ends of the same act would still typecheck if a caller swapped them.
data BecameUnattached = MkBecameUnattached
  { -- | The permanent that became unattached -- rule 701.3d's Aura, Equipment or
    -- Fortification. The id it had while it was attached, which is the key its
    -- CR 608.2h last known information is filed under when the route was the
    -- attachment leaving the battlefield.
    attachment :: ObjectId.ObjectId,
    -- | What it became unattached FROM. A Recipient for BecameAttached's reason:
    -- rule 701.3d says "from that object or player", and CR 702.5a's enchant
    -- ability can name a player.
    host :: Recipient.Recipient
  }
  deriving (Eq, Ord, Show)
