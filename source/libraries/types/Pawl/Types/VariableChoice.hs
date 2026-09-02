module Pawl.Types.VariableChoice where

-- | CR 107.3: whose the value of a spell's X is, for the cost CR 601.2b's
-- announcement settles on.
data VariableChoice
  = -- | CR 107.3a: the controller chooses and announces X.
    Announced
  | -- | CR 107.3b: paying neither the mana cost nor an alternative cost that
    -- includes X, so 0 is the only legal X.
    FixedAtZero
  deriving (Eq, Ord, Show)
