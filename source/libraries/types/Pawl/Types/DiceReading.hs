module Pawl.Types.DiceReading where

-- | CR 706.4: how an instruction that rolls several dice reads their results
-- into Pawl.Types.RollDie's `slot`.
data DiceReading
  = -- | CR 706.4: the roller chooses one result (Valiant Endeavor).
    ChooseOne
  | -- | CR 706.4: the total of every result (Neverwinter Hydra).
    Total
  | -- | CR 706.4: each of two results on its own, the first into `slot` and
    -- the second into `other`, for CR 706.5's doubles (Celebr-8000).
    Both
  deriving (Bounded, Enum, Eq, Ord, Show)
