module Pawl.Types.CounterCause where

-- | CR 614.16: what is putting counters on, at the ONE grain that rule cares
-- about. Its replacement effects -- "if an effect would put one or more counters
-- on a permanent" -- "apply if the effect of a resolving spell or ability puts a
-- counter on a permanent, and they also apply if another replacement or
-- prevention effect does so, even if the original event being modified wasn't
-- itself an effect."
--
-- So the question is not who placed the counters but whether an EFFECT did, which
-- CR 609.1 defines: "An effect is something that happens in the game as a result
-- of a spell or ability." A turn-based action is the result of neither.
--
-- Carried into Pawl.Engine.Event.putCounters rather than derived there, because
-- nothing about a proposed placement says where it came from -- the object, the
-- kind and the count are identical either way.
data CounterCause
  = -- | A resolving spell or ability's effect (CR 609.1), or another replacement
    -- or prevention effect -- CR 614.16's two admitted causes, which it treats
    -- alike. Pawl.Engine.Resolve's PutCounters and proliferate arms are the first
    -- kind; CR 714.3a's Saga entry and CR 702.136a's riot are the second.
    ByEffect
  | -- | A rule, acting on its own. CR 714.3c's turn-based action is the only one
    -- today, and it is exactly the case CR 614.16 excludes: Doubling Season does
    -- NOT double the lore counter a Saga gets as its controller's precombat main
    -- phase begins, though it does double the one the Saga entered with.
    ByRule
  deriving (Eq, Ord, Show)
