module Pawl.Types.SuspendCounters where

import qualified Numeric.Natural as Natural

-- | CR 702.62a's N in "Suspend N--[cost]": how many time counters the special
-- action exiles the card with.
--
-- TWO ARMS rather than a Natural because on some printings the N and the {X} in
-- the cost beside it are the same letter (Benalish Commander's "Suspend
-- X--{X}{W}{W}"), and CR 107.3d has the player choose that value immediately
-- before paying. CR 107.3i is what makes the two halves one number.
--
-- Tagged on the wire for Pawl.Types.Loyalty's reason: a chosen X is a wire value
-- rather than a sentinel number the decoder would have to reserve.
data SuspendCounters
  = -- | CR 702.62a's printed numeral (Rift Bolt's "Suspend 1--{R}").
    Literal Natural.Natural
  | -- | CR 107.3d's chosen X, whose payload is the LEAST value the card's own
    -- words allow -- 1 under "X can't be 0", which CR 101.2 lets beat rule
    -- 107.3d's otherwise free choice.
    Variable Natural.Natural
  deriving (Eq, Ord, Show)

-- | The floor CR 107.3d's announcement may not go under. Zero where the N is a
-- numeral, which declares no X at all -- so this is the value
-- Pawl.Engine.Suspend prices the special action at before anything is announced,
-- CR 601.2b's X=0 floor one rule over.
leastX :: SuspendCounters -> Natural.Natural
leastX counters = case counters of
  Literal _ -> 0
  Variable n -> n
