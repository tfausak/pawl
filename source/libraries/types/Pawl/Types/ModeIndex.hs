module Pawl.Types.ModeIndex where

import qualified Numeric.Natural as Natural

-- | CR 700.2: a mode is one option in the printed bulleted list. Modes have no
-- meaningful label to conjure (unlike a target SlotName), so a mode is referenced
-- by its ORDINAL -- and the ordinal is load-bearing, not incidental: CR 608.2c
-- resolves modes in the order written, CR 700.2d treats a mode chosen twice as
-- "appear[ing] that many times in sequence," CR 700.2g copies modes by position.
-- A newtype, not a bare Natural, so the reference is typed. Ord is load-bearing:
-- ModeIndex is a Set element (the chosen modes) and its ordering IS printed order.
newtype ModeIndex = MkModeIndex
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
