module Pawl.Types.BecameTarget where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 601.2c: one object BECOMING A TARGET of a spell or ability -- "the chosen
-- objects and\/or players each become a target of that spell. (Any abilities that
-- trigger when those objects and\/or players become the target of a spell trigger
-- at this point ...)". Recorded by Pawl.Engine.Event.becameTarget, the one funnel
-- every announcement goes through, once per targeted object. CR 602.2b and CR
-- 603.3d put an activated and a triggered ability through the same rule.
--
-- A record rather than positional fields, Pawl.Types.Countering's reason: two of
-- the three are ObjectIds and they are the two ends of the same act, so a caller
-- that swapped them would still typecheck.
--
-- Only an OBJECT target is recorded. Rule 601.2c names a targeted PLAYER in the
-- same breath, and no card in the pool watches for that (#1524); the sibling that
-- would carry one is a Recipient in place of `targeted` rather than a second
-- event.
data BecameTarget = MkBecameTarget
  { -- | The object that became a target -- the permanent CR 702.21a's ward is on.
    -- The live id on the stack or battlefield, as of the announcement.
    targeted :: ObjectId.ObjectId,
    -- | The spell or ability that named it. CR 701.6a's countering takes this id,
    -- so it is the STACK object either way: the spell itself for a spell, and the
    -- ability object -- not CR 113.7's source permanent -- for an ability, since
    -- that is what a ward trigger counters.
    source :: ObjectId.ObjectId,
    -- | CR 405.4: who controlled `source` as it was put onto the stack. The player
    -- rule 702.21a's "an opponent controls" compares against CR 109.5's "you", and
    -- the one rule 702.21a offers the ward cost to.
    --
    -- Captured here rather than re-derived at match time, for
    -- Countering.controller's reason: the targeting object can leave the stack
    -- before anything reads this, and re-deriving would then read control at the
    -- scan boundary rather than at the event.
    controller :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
