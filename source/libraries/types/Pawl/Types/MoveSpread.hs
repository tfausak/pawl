module Pawl.Types.MoveSpread where

-- | How many counters a CR 122.5 move onto a group of permanents carries in
-- all, as Pawl.Types.Prompt's ChooseDistributedMovedCounters states it: the
-- answer says where each counter lands, and this says what total it must reach.
data MoveSpread
  = -- | Forgotten Ancient's "any number": up to the offered tallies, none
    -- included (CR 122.5).
    AnyNumber
  | -- | "One or more": up to the offered tallies, at least one counter in all
    -- (CR 122.5).
    AtLeastOne
  | -- | "All +1\/+1 counters": every counter of the offered tallies, the card
    -- having settled the batch (CR 122.5).
    Exactly
  deriving (Bounded, Enum, Eq, Ord, Show)
