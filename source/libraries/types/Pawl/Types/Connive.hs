module Pawl.Types.Connive where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity

-- | The payload of Pawl.Types.Effect's Connive arm: CR 701.50d's "connive N",
-- plus the reference naming the permanents that connive.
--
-- CR 701.50a's bare "connive" is CR 701.50d with N of one, so every printing
-- writes a Quantity here and the rule's two spellings share one arm.
--
-- N is a Quantity rather than a Natural, Pawl.Types.Amass's reason: Raffine,
-- Scheming Seer prints "connive X, where X is the number of attacking
-- creatures", so the count is an expression over game state.
data Connive = MkConnive
  { quantity :: Quantity.Quantity,
    ref :: ObjectRef.ObjectRef
  }
  deriving (Eq, Ord, Show)
