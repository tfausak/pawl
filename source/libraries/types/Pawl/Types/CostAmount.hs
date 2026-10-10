module Pawl.Types.CostAmount where

import qualified Numeric.Natural as Natural

-- | How much of a resource one Pawl.Types.CostComponent asks for: a printed
-- number, or CR 107.3a's X, which CR 601.2b announces and
-- Pawl.Engine.Cost.substituteX fixes.
data CostAmount
  = -- | A number printed on the card, or one an announcement already fixed.
    Fixed Natural.Natural
  | -- | CR 107.3a / 601.2b: X, not yet announced.
    AnnouncedX
  deriving (Eq, Ord, Show)
