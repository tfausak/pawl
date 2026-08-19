module Pawl.Types.BecameTarget where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.StackObjectKind as StackObjectKind

-- | CR 601.2c: one object or player BECOMING A TARGET of a spell or ability --
-- "the chosen objects and\/or players each become a target of that spell. (Any
-- abilities that trigger when those objects and\/or players become the target of
-- a spell trigger at this point ...)". Recorded by Pawl.Engine.Event.becameTarget,
-- the one funnel every announcement goes through, once per targeted recipient.
-- CR 602.2b and CR 603.3d put an activated and a triggered ability through the
-- same rule, which is what `kind` distinguishes.
--
-- A record rather than positional fields, Pawl.Types.Countering's reason: `source`
-- and the ObjectId inside `targeted` are the two ends of the same act, so a
-- caller that swapped them would still typecheck.
data BecameTarget = MkBecameTarget
  { -- | What became a target -- the permanent CR 702.21a's ward is on, or CR
    -- 115.1's targeted PLAYER, which that rule makes a target in its own right.
    -- A Recipient rather than an ObjectId because rule 601.2c names the two in
    -- one breath, so one event with a Recipient beats two events. The live id on
    -- the stack or battlefield, as of the announcement.
    targeted :: Recipient.Recipient,
    -- | The spell or ability that named it. CR 701.6a's countering takes this id,
    -- so it is the STACK object either way: the spell itself for a spell, and the
    -- ability object -- not CR 113.7's source permanent -- for an ability, since
    -- that is what a ward trigger counters.
    source :: ObjectId.ObjectId,
    -- | Which of CR 601.2c's two roads `source` came by. Dormant Gomazoa reads
    -- "the target of A SPELL" (CR 112.1) where ward and Amulet of Safekeeping read
    -- "a spell or ability",
    -- and the matcher that answers both is pure with no GameState, so the
    -- classification has to ride on the event rather than be looked up from it.
    kind :: StackObjectKind.StackObjectKind,
    -- | CR 405.4: who controlled `source` as it was put onto the stack. The player
    -- an "an opponent controls" clause compares against CR 109.5's "you" -- rule
    -- 702.21a's ward and Amulet of Safekeeping's alike -- and the one each of them
    -- offers its cost to.
    --
    -- Captured here rather than re-derived at match time, for
    -- Countering.controller's reason: the targeting object can leave the stack
    -- before anything reads this, and re-deriving would then read control at the
    -- scan boundary rather than at the event.
    controller :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
