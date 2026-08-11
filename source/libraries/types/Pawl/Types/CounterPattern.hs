module Pawl.Types.CounterPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.CounterKind as CounterKind
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
-- (`byWhom`), where Doubling Season's "on a permanent you control" narrows by
-- whose permanent RECEIVES them (`whose`). Its own ruling says the card "cares
-- deeply about who is putting the counters", and the two players differ whenever
-- one player's effect puts counters on another's permanent.
data CounterPattern = MkCounterPattern
  { whichKind :: Maybe (CounterKind.CounterKind Keyword.Keyword),
    -- | CR 109.5: who is PUTTING the counters, read against the controller of the
    -- effect's source -- or Nothing for CR 614.16's own subject, "if an EFFECT
    -- would put", which names no player. Doubling Season, Hardened Scales and
    -- Corpsejack Menace all take Nothing; Vorinclex is Just Yours and Just
    -- Opponents.
    --
    -- The distinction is not decoration: rule 614.16's effects reach only what a
    -- resolving spell, ability, replacement or prevention effect puts on, where a
    -- clause naming a player also reaches CR 714.3c's turn-based lore counter --
    -- which a player puts. See Pawl.Engine.Replacement.matchesPutter.
    byWhom :: Maybe ControllerRelation.ControllerRelation,
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
