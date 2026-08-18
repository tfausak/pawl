module Pawl.Types.BecameAttached where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 701.3a: one Aura, Equipment or Fortification BECOMING ATTACHED to an
-- object or player -- "to take it from where it currently is and put it onto
-- that object or player". Recorded by Pawl.Engine.Event.attach, the funnel every
-- CR 701.3 attachment goes through, and by that module's zone-change path for CR
-- 608.3c's resolving Aura spell and CR 303.4f's entry choice, which put a
-- permanent onto the battlefield already attached rather than moving one that is
-- there.
--
-- A record rather than positional fields, Pawl.Types.Mentored's reason: the
-- ObjectId inside `host` and `attachment` are the two ends of the same act, so a
-- caller that swapped them would still typecheck.
data BecameAttached = MkBecameAttached
  { -- | The permanent that became attached -- rule 701.3a's Aura, Equipment or
    -- Fortification. The live id, as of the attachment: for an entry this is the
    -- CR 400.7 incarnation on the battlefield rather than the id the spell had
    -- on the stack.
    attachment :: ObjectId.ObjectId,
    -- | What it became attached TO. A Recipient and not an ObjectId, because rule
    -- 701.3a says "to an object or player" and CR 702.5a lets an enchant ability
    -- name a player. Pawl.Types.BecameTarget.targeted is the house style this
    -- follows.
    host :: Recipient.Recipient
  }
  deriving (Eq, Ord, Show)
