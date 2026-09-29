module Pawl.Types.DoesNotUntapNext where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | CR 502.3 / 611.2a: the permanents the ObjectRef names don't untap during
-- their controller's next `steps` untap steps.
data DoesNotUntapNext = MkDoesNotUntapNext
  { ref :: ObjectRef.ObjectRef,
    -- | 1 for "next untap step" (Elvish Hunter), 2 for "next two untap steps"
    -- (Telekinesis).
    steps :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
