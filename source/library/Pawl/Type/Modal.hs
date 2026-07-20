module Pawl.Type.Modal where

import Data.Sequence (Seq)
import Pawl.Type.Mode (Mode)
import Pawl.Type.ModeSelection (ModeSelection)

-- CR 700.2: a spell's or ability's modal payload. `modes` is a Seq -- ordered
-- (printed order, indexed by ModeIndex) and NON-EMPTY by invariant (a payload has
-- at least one mode; the codec rejects an empty modes array, the UnsafeX/textToX
-- posture -- there is no NonEmpty Seq in base). A non-modal payload is one Mode with
-- ChooseExactly 1.
data Modal card = MkModal
  { modes :: Seq (Mode card),
    selection :: ModeSelection
  }
  deriving (Eq, Ord, Show)
