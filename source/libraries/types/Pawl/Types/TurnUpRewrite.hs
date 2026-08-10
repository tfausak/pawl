module Pawl.Types.TurnUpRewrite where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 614.1e: how an "As [this permanent] is turned face up . . ." replacement
-- modifies the turning-over. EntryRewrite's twin one event class over, and a
-- separate type rather than a reuse of it: CR 614.1c's rewrites include entering
-- as a copy (CR 707.5), entering under another player's control (CR 616.1b) and
-- entering tapped (CR 110.5b), none of which a permanent already on the
-- battlefield can be made to do by turning over.
--
-- TWO constructors, one per shape the pool prints. CR 208.2b names a third the
-- rules allow -- a static ability that "sets the creature's power and toughness
-- ... as it enters the battlefield or is turned face up" -- and that is a
-- constructor here plus a layer-1a write, not a change to either of these.
data TurnUpRewrite
  = -- | CR 702.37b: "put a +1/+1 counter on it". The kind and the count ride the
    -- constructor, as EntryRewrite.WithCounters' do, and the counters are placed
    -- through Pawl.Engine.Event.putCounters -- CR 122.6's funnel -- so CR 614.16
    -- applies and Doubling Season doubles a megamorph's counter.
    WithCounters (CounterKind.CounterKind Keyword.Keyword) Natural.Natural
  | -- | CR 303.4k: "you may attach it to a creature" (Gift of Doom), the one
    -- printing of the "effect [that] allows an Aura that's being turned face up
    -- to become attached" that rule is conditional on.
    --
    -- The Filter is the card's own destination TEXT and nothing more -- "a
    -- creature" -- because CR 303.4k supplies the rest itself: "the Aura's
    -- controller considers the characteristics of that Aura as it would exist if
    -- it were face up ... and they must choose a legal object or player
    -- according to the Aura's enchant ability". Pawl.Engine.Attach.turnUpHosts
    -- adds that enchant-ability conjunct, which is why Filter.CanHostSubject
    -- does not appear in the card data and must not be written into it. That is
    -- the difference from Effect.AttachTarget, where the narrowing happens only
    -- when the card SAYS "it can enchant" and CR 303.4j is the backstop
    -- otherwise: rule 303.4k leaves no such backstop open.
    --
    -- A "MAY", spelled into the constructor's name rather than carried as a
    -- flag, because that is the only form CR 303.4k's producer takes. A
    -- mandatory one would be a second constructor.
    MayAttachTo (Filter.Filter Keyword.Keyword)
  deriving (Eq, Ord, Show)
