module Pawl.Types.CounterSubject where

import qualified Pawl.Types.ControllerRelation as ControllerRelation

-- | CR 614.1 / 614.16: the SUBJECT of a counter-scaling replacement's clause --
-- what its "would put" is said of. Printed wordings take three shapes and they
-- reach different placements, so the subject is data rather than a default.
--
-- Read by Pawl.Engine.Replacement.matchesPutter against the
-- Pawl.Types.CounterCause riding the event, and by nothing else.
data CounterSubject
  = -- | "If an EFFECT would put one or more counters on a permanent you control"
    -- -- Doubling Season. CR 614.16 admits exactly two causes for this subject,
    -- a resolving spell or ability's effect and another replacement or prevention
    -- effect, and CR 609.1 makes a turn-based action neither. So this subject
    -- misses CR 714.3c's lore counter and CR 120.3's damage results.
    ByEffect
  | -- | "If YOU would put", "if an OPPONENT would put" -- Vorinclex, Monstrous
    -- Raider. CR 109.5 reads the relation against the controller of the effect's
    -- source. Every cause names a player, so this subject reaches all of them,
    -- narrowed by which player that is.
    ByPlayer ControllerRelation.ControllerRelation
  | -- | "If one or more +1/+1 counters would be put on a creature you control" --
    -- Hardened Scales, Corpsejack Menace, Vizier of Remedies. The clause names
    -- neither an effect nor a player, so CR 614.1's general reading applies and
    -- every placement is reached, including one the game performs by rule. The
    -- widest of the three, and the one that tells Vizier of Remedies apart from
    -- the other two: the -1/-1 counters CR 120.3d's wither and infect damage
    -- leave are put on by a rule, so ByEffect would miss them.
    ByAnything
  deriving (Eq, Ord, Show)
