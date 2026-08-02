module Pawl.Types.Modal where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection

-- | CR 700.2: a spell's or ability's modal payload. `modes` is a Seq -- ordered
-- (printed order, indexed by ModeIndex) and NON-EMPTY by invariant (a payload has
-- at least one mode; the codec rejects an empty modes array, the UnsafeX/textToX
-- posture -- there is no NonEmpty Seq in base). A non-modal payload is one Mode with
-- ChooseExactly 1.
data Modal card = MkModal
  { modes :: Seq.Seq (Mode.Mode card),
    selection :: ModeSelection.ModeSelection
  }
  deriving (Eq, Ord, Show)
