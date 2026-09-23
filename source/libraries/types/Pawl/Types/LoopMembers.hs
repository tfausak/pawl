module Pawl.Types.LoopMembers where

-- | Which of the players and objects a CR 608.2f loop sweeps it actually runs
-- over (Pawl.Types.ForEach).
data LoopMembers
  = -- | Every swept member, CR 608.2f's "for each ...".
    Every
  | -- | Any number of them, chosen once by the resolving controller before the
    -- first iteration (CR 608.2d) -- rule 702.116a's "for each opponent ..., you
    -- may".
    AnyNumber
  deriving (Bounded, Enum, Eq, Ord, Show)
