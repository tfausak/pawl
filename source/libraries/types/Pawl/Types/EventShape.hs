module Pawl.Types.EventShape where

import Pawl.Types.Zone (Zone)

-- Which recorded events a history count folds over. GameState.events is cleared
-- at the turn change (Pawl.Engine.Engine), an engine choice made under CR 608.2i
-- because every history-reading card in the pool asks "this turn" -- so the
-- log's extent IS the window and none is carried here.
--
-- Only MovedBetween exists: every OTHER GameEvent constructor -- DamageDealt,
-- StepBegan, SpellCast, BecameMonarch, Discarded, AttackerDeclared and Revealed
-- -- is recorded in
-- the log with no EventShape arm, so a count cannot fold over any of them
-- (#162). Revealed is the one that already carries a characteristics snapshot,
-- so it is the one an arm here could match a Filter against without any new
-- payload.
data EventShape
  = -- CR 700.4: "dies" means "is put into a graveyard from the battlefield",
    -- which is MovedBetween Battlefield Graveyard.
    MovedBetween Zone Zone
  deriving (Eq, Ord, Show)
