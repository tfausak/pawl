module Pawl.Types.EventGroup where

import qualified Numeric.Natural as Natural

-- | CR 704.3 / CR 608.2f: which recorded events were ONE event. Every entry in
-- GameState.events carries one, and two entries are simultaneous exactly when
-- their groups are equal.
--
-- The rules distinguish "several events in a row" from "one event affecting
-- several objects" -- CR 704.3 performs all applicable state-based actions
-- "simultaneously as a single event", CR 608.2f processes a spell's actions on
-- multiple objects simultaneously -- and a flat log cannot recover the
-- difference. CR 603.10a is where that costs an answer rather than being a
-- distinction without one: its look-back reads "the appearance of objects
-- immediately prior to the event", and its own Example turns on a permanent that
-- "goes to its owner's graveyard AT THE SAME TIME AS the creatures".
--
-- OPAQUE, and deliberately so. The only question anything asks of two groups is
-- whether one is the same as, or later than, the other; nothing reads the number
-- and nothing is stamped with a group chosen for its value. The Ord instance is
-- that comparison and the Natural is only how a fresh one is minted.
--
-- Allocated by Pawl.Engine.Event.recordEvent, one per event, and FROZEN for the
-- duration of Pawl.Engine.Event.simultaneously -- which is the bracket a rules
-- site puts around a body the CR says is one event. Gaps in the sequence are
-- expected and mean nothing: a bracket that recorded no events still spends its
-- group.
newtype EventGroup = MkEventGroup
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | The group a game's first event is stamped with. Groups only ever increase
-- within a game, so this is a starting point rather than a value anything
-- compares against.
first :: EventGroup
first = MkEventGroup 0

-- | The next group to mint. Not exported as an Enum instance: succ is the only
-- arithmetic anything does on a group, and an Enum would offer the rest.
next :: EventGroup -> EventGroup
next = MkEventGroup . (+ 1) . unwrap
