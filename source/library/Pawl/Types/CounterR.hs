module Pawl.Types.CounterR where

import qualified Pawl.Types.CounterPattern as CounterPattern
import qualified Pawl.Types.Scaling as Scaling

-- | The payload of Pawl.Types.ReplacementEffect's CounterR arm (#1305): which
-- counter placements are intercepted, and how the number placed is scaled.
data CounterR = MkCounterR
  { matching :: CounterPattern.CounterPattern,
    scaling :: Scaling.Scaling
  }
  deriving (Eq, Ord, Show)
