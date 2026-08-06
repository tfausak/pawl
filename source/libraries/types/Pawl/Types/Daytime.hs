module Pawl.Types.Daytime where

-- | CR 731.1: "Day and night are designations that the game itself can have."
-- Two constructors and not three, because the rule's third state -- "the game
-- starts with neither designation" -- is the ABSENCE of one: it is spelled
-- `Nothing` wherever a designation is held (GameState.daytime), which is what
-- makes "neither" unrepresentable as a value that could be assigned by mistake.
-- CR 731.1's last sentence is why that matters: "once it has become day or
-- night, the game will have exactly one of those designations from that point
-- forward", so the neither-state is never returned to and nothing may write it.
--
-- Named for the DESIGNATION, not for a phase or a step: rule 731 puts it on the
-- game, so it is read the way GameState.monarch is (CR 725.1) rather than the
-- way GameState.phase is.
-- CR 731.1a's "day becomes night" needs no operation of its own: the rule makes
-- it the game losing the first designation and gaining the second, which is one
-- assignment of the other constructor (Pawl.Engine.Daytime.becomes).
data Daytime
  = Day
  | Night
  deriving (Eq, Ord, Show)
