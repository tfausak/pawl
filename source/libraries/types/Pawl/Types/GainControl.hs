module Pawl.Types.GainControl where

import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | "That player gains control of these objects, for this long" -- the payload
-- of Pawl.Types.Effect's GainControl arm (CR 613.1b, CR 611.2c).
data GainControl = MkGainControl
  { duration :: Duration.Duration,
    ref :: ObjectRef.ObjectRef,
    -- | The new controller, who must be exactly one player: the effect's
    -- controller for Zealous Conscripts, an opponent for Jinxed Idol, a
    -- teammate for CR 804.2's deploy.
    to :: PlayerRef.PlayerRef
  }
  deriving (Eq, Ord, Show)
