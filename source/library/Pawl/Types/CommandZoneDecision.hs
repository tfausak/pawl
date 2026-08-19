module Pawl.Types.CommandZoneDecision where

-- | CR 903.9a: a commander's owner's answer to "this commander is in a graveyard
-- or in exile; put it into the command zone?"
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes, so a transcript reads as the decision it records.
--
-- Its own type rather than a reuse of EntwineDecision or OptionalDecision, which
-- are answered while a spell is being CAST (CR 601.2b) and as one RESOLVES (CR
-- 603.5). This one is answered during a state-based action check (CR 704), which
-- is neither.
data CommandZoneDecision
  = Leaves
  | Returns
  deriving (Eq, Ord, Show)
