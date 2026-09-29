module Pawl.Types.RepeatIf where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Condition as Condition

-- | CR 608.2c's "if [condition], [instructions] and repeat this process":
-- Grist, the Hunger Tide's +1. No player chooses whether to go again, which is
-- what separates it from Pawl.Types.Effect's Repeat.
--
-- Parametric in the effect for Pawl.Types.ForEach's reason.
data RepeatIf effect = MkRepeatIf
  { -- | The process, run in written order (CR 608.2c) before each check.
    process :: Seq.Seq effect,
    -- | Asked after each run of the process, against the state it left.
    condition :: Condition.Condition,
    -- | What the "if" also does before the next run, in written order.
    ifHolds :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
