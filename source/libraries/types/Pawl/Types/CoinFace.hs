module Pawl.Types.CoinFace where

-- | CR 705.1: which side of a flipped coin landed face up. The rule allows any
-- two-sided object and any equally likely substitute, and says that a coin
-- without an obvious heads or tails has one side DESIGNATED heads and the other
-- tails -- so the two names are the designation, not a claim about a physical
-- coin.
--
-- A named sum rather than a Bool, the posture every player-facing two-way answer
-- in this engine takes (Pawl.Types.OptionalDecision), so a transcript reads as
-- the face it records.
--
-- One type for BOTH halves of CR 705.2: the face the coin came up
-- (Pawl.Types.Prompt's FlipCoin) and the face the flipping player called
-- (Prompt's CallCoin). They are the same vocabulary -- the rule compares them
-- for equality -- and it is Prompt and Pawl.Types.Response that keep an announced
-- call from satisfying a question that asked randomness.
data CoinFace
  = Heads
  | Tails
  deriving (Eq, Ord, Show)
