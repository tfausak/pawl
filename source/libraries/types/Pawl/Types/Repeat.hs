module Pawl.Types.Repeat where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 608.2d's "may repeat this process": the body, then a player's choice
-- whether to run it again -- Kindle the Carnage's "you", Trade Secrets' "that
-- opponent".
--
-- Parametric in the effect for Pawl.Types.ForEach's reason.
data Repeat effect = MkRepeat
  { -- | Who is asked after each run; every printing names one player.
    chooser :: PlayerRef.PlayerRef,
    -- | The process, run in written order (CR 608.2c) before each ask.
    body :: Seq.Seq effect
  }
  deriving (Eq, Ord, Show)
