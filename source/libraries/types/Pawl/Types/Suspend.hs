module Pawl.Types.Suspend where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost

-- | The payload of Pawl.Types.Keyword's Suspend arm: CR 702.62a's "Suspend
-- N--[cost]", the two halves of that one printed line.
--
-- PARAMETRIC in the keyword, for Pawl.Types.Equip's reason: `cost` names a Cost,
-- which can name a Keyword, and Keyword names THIS. Only
-- @Suspend Keyword.Keyword@ is ever written.
--
-- `counters` is rule 702.62a's N, the number of time counters the special action
-- exiles the card with, and `cost` is what that action charges. Two fields
-- rather than two constructors because rule 702.62a states them in one sentence
-- and the special action needs both at once.
--
-- `counters` is a settled Natural and so cannot state CR 107.3d's chosen X,
-- which the five printed "Suspend X" cards write into the N and the cost at once
-- (gap #3360).
data Suspend keyword = MkSuspend
  { counters :: Natural.Natural,
    cost :: Cost.Cost keyword
  }
  deriving (Eq, Ord, Show)
