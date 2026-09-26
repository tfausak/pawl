module Pawl.Types.DiceReading where

-- | CR 706.4: how an instruction that rolls several dice reads their results
-- into Pawl.Types.RollDie's `slot`.
--
-- Not implemented: CR 706.5's doubles, the reading that compares the results
-- rather than choosing or adding them (#3243).
data DiceReading
  = -- | CR 706.4: the roller chooses one result (Valiant Endeavor).
    ChooseOne
  | -- | CR 706.4: the total of every result (Neverwinter Hydra).
    Total
  deriving (Bounded, Enum, Eq, Ord, Show)
