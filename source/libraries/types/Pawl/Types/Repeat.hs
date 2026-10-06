module Pawl.Types.Repeat where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 608.2d's "may repeat this process": the body, then a player's choice
-- whether to run it again -- Kindle the Carnage's "you", Trade Secrets' "that
-- opponent".
--
-- Parametric in the effect for Pawl.Types.ForEach's reason.
data Repeat effect = MkRepeat
  { -- | Who is asked after each run.
    chooser :: PlayerRef.PlayerRef,
    -- | The process, run in written order (CR 608.2c) before each ask.
    body :: Seq.Seq effect,
    -- | CR 706.3c: what must hold, after a run, for the ask to be made at all
    -- -- Delina, Wild Mage's "15--20 | ... You may roll again". Absent asks
    -- every time.
    gate :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
