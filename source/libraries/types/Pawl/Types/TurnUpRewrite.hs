module Pawl.Types.TurnUpRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind

-- | CR 614.1e: how an "As [this permanent] is turned face up . . ." replacement
-- modifies the turning-over. EntryRewrite's twin one event class over, and a
-- separate type rather than a reuse of it: CR 614.1c's rewrites include entering
-- as a copy (CR 707.5), entering under another player's control (CR 616.1b) and
-- entering tapped (CR 110.5b), none of which a permanent already on the
-- battlefield can be made to do by turning over.
--
-- ONE constructor, because rule 702.37b's megamorph counter is the whole of what
-- the pool asks for today. CR 208.2b names the other shape the rules allow --
-- a static ability that "sets the creature's power and toughness ... as it
-- enters the battlefield or is turned face up" -- and that is a second
-- constructor here plus a layer-1a write, not a change to this one.
data TurnUpRewrite
  = -- | CR 702.37b: "put a +1/+1 counter on it". The kind and the count ride the
    -- constructor, as EntryRewrite.WithCounters' do, and the counters are placed
    -- through Pawl.Engine.Event.putCounters -- CR 122.6's funnel -- so CR 614.16
    -- applies and Doubling Season doubles a megamorph's counter.
    WithCounters CounterKind.CounterKind Natural.Natural
  deriving (Eq, Ord, Show)
