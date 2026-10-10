module Pawl.Types.PermanentAction where

-- | Something a permanent does that a "whenever [a permanent] <acts>" trigger
-- (CR 603.2) watches, with no payload the trigger reads. Recorded by
-- Pawl.Types.GameEvent's PermanentActed and matched by
-- Pawl.Types.TriggerCondition's PermanentActs. Each constructor's comment names
-- the rule that places the moment.
data PermanentAction
  = -- | CR 702.100b: only when the evolve ability put one or more counters.
    Evolve
  | -- | CR 702.140d: a mutating creature spell merged with it.
    Mutate
  | -- | CR 702.149c: only when the training ability put one or more counters.
    Train
  | -- | CR 701.43a, only where the exert was paid, so CR 701.43d's "when you
    -- do" links by construction; a second exert is a second act (CR 701.43b).
    Exert
  | -- | CR 701.44b, even where some or all of the explore was impossible.
    Explore
  | -- | CR 701.50f; CR 701.50e's connive 0 is none.
    Connive
  | -- | CR 701.27b: a face-up permanent was turned face down (CR 708.2).
    TurnFaceDown
  deriving (Bounded, Enum, Eq, Ord, Show)
