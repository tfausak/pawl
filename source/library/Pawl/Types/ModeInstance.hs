module Pawl.Types.ModeInstance where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ModeIndex as ModeIndex

-- | CR 700.2d: one INSTANCE of a chosen mode -- which mode it is, and which
-- occurrence of that mode this is among the ones chosen (0-based, so a mode
-- chosen once is always occurrence 0).
--
-- A mode chosen twice is not the same thing twice: "the spell is treated as if
-- that mode appeared that many times in sequence", and "if that mode requires a
-- target, the same player or object may be chosen as the target for each of
-- those modes, or different targets may be chosen". Two independent targets need
-- two independent slots, and a 'ModeIndex' alone cannot name them apart, so this
-- is the key Pawl.Engine.Modal qualifies a mode's slot names by.
--
-- Ord is load-bearing, and its field order with it: modes resolve in ModeIndex
-- order (CR 608.2c), with a repeated mode's occurrences adjacent and in order,
-- which is exactly this type's ascending order.
data ModeInstance = MkModeInstance
  { index :: ModeIndex.ModeIndex,
    occurrence :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
