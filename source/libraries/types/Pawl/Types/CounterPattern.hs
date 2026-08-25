module Pawl.Types.CounterPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterSubject as CounterSubject
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 122.6 / 614.1: which counter placements a scaling replacement intercepts.
-- Hardened Scales names a kind and a real filter; Doubling Season's counter clause
-- names no kind and takes `And []`, the trivial filter matching every permanent.
-- `whichKind = Nothing` means ANY kind, never "no kind" -- the two cards differ by
-- data, and neither is a constructor.
--
-- Two axes, not one, and Vorinclex, Monstrous Raider is why: "if YOU would put
-- one or more counters on a permanent or player" narrows by who is PUTTING them
-- (`subject`), where Doubling Season's "on a permanent you control" narrows by
-- whose permanent RECEIVES them (`whose`). Its own ruling says the card "cares
-- deeply about who is putting the counters", and the two players differ whenever
-- one player's effect puts counters on another's permanent.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    -- | CR 614.1: what the clause says its "would put" OF -- an effect, a player,
    -- or nothing at all. See Pawl.Types.CounterSubject, which carries the three
    -- printed shapes and what each reaches, and
    -- Pawl.Engine.Replacement.matchesPutter, which answers it against the cause
    -- riding the event.
    --
    -- Not a Maybe and not defaulted in the codec: which subject a card names is
    -- the difference between Doubling Season leaving CR 714.3c's lore counter
    -- alone and Vizier of Remedies shrinking what CR 120.3d's wither damage
    -- leaves, and a silently narrow default is what made two cards narrower than
    -- printed; see #1232.
    subject :: CounterSubject.CounterSubject,
    -- | CR 109.5: whose PERMANENT receives them.
    whose :: ControllerRelation.ControllerRelation,
    -- | Which permanents receive them.
    onWhat :: Filter.Filter Keyword.Keyword,
    -- | CR 122.1: which PLAYERS receive them, or Nothing for a pattern that
    -- reaches permanents only (Doubling Season -- "a permanent you control"
    -- cannot be a player). Vorinclex's "or player" is Just Anyones.
    --
    -- A relation rather than a Filter, because no printing narrows the receiving
    -- player by anything but CR 109.5's "you" and "an opponent"; and it does not
    -- pair with a PlayerCounterKind, because `whichKind` is the OBJECT kinds and
    -- the two domains are disjoint (see Pawl.Types.PlayerCounterKind). A pattern
    -- naming a kind therefore reaches no player at all, which is right for every
    -- printing today: Hardened Scales says "+1/+1 counters", a kind no player can
    -- have.
    onWho :: Maybe ControllerRelation.ControllerRelation
  }
  deriving (Eq, Ord, Show)
