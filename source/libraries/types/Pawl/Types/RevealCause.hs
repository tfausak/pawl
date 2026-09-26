module Pawl.Types.RevealCause where

import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

-- | CR 701.20a: why a reveal happened. Carried by Pawl.Types.GameEvent's Revealed
-- for Pawl.Types.DiscardCause's reason -- one logged reveal answers both of the
-- questions the rules ask about it, rather than two log entries describing one
-- showing of one card.
--
-- CR 702.94a is what makes the distinction load-bearing. Miracle's linked
-- triggered ability (CR 603.11) is worded "when you reveal this card THIS WAY",
-- so it must not fire when the same card is shown by some other effect. A cause
-- on the event is how the trigger condition asks that, and it is the same shape
-- CR 702.29d needed one rule over.
data RevealCause
  = -- | CR 701.20a's plain reveal, whatever asked for it: a search's "reveal it",
    -- an activation cost paid from a hidden zone, a library's top card turned
    -- face up.
    Ordinary
  | -- | CR 702.94a / CR 121.9 / 702.94b: revealed from a hand AS IT WAS DRAWN,
    -- under the miracle ability with this cost, whose linked trigger alone it
    -- fires (CR 607.2h).
    ForMiracle (Cost.Cost Keyword.Keyword)
  deriving (Eq, Ord, Show)
