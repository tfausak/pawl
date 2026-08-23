module Pawl.Types.LoggedEvent where

import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.GameEvent as GameEvent

-- | One entry in CR 608.2i's log of what happened this turn: the event, and the
-- group it belongs to (CR 704.3 / CR 608.2f's "single event"), stamped by
-- Pawl.Engine.Event.recordEvent.
--
-- A record rather than a pair, because the log is scanned by two watermarks at
-- different cadences and every reader has to say which half it means.
-- Pawl.Types.GameState.events is the log itself, and carries why it stays flat.
data LoggedEvent = MkLoggedEvent
  { group :: EventGroup.EventGroup,
    event :: GameEvent.GameEvent
  }
  deriving (Eq, Ord, Show)
