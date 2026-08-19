module Pawl.Types.PaymentDecision where

-- | CR 118.12a: a player's answer to a cost a resolving spell or ability offers
-- them -- Mana Leak's "unless its controller pays {3}", which that rule rewrites
-- as "its controller may pay {3}. If they don't, counter it."
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes, so a transcript reads as the decision it records.
--
-- Its own type rather than a reuse of OptionalDecision, which is scoped to CR
-- 603.5's printed "may" and is asked of the RESOLVING object's controller. This
-- one is asked of whichever player the card names -- for Mana Leak the targeted
-- spell's controller, who controls nothing about the resolution.
--
-- The distinction CR 118.12 turns on is why this is a DECISION rather than a
-- Pawl.Types.Payment: that rule's "If [a player] [does, doesn't, or can't]"
-- clause "checks whether the player chose to pay an optional cost or started to
-- pay a mandatory cost, regardless of what events actually occurred", so what
-- the branch reads is this answer and not the payment's outcome.
data PaymentDecision
  = Declines
  | Pays
  deriving (Eq, Ord, Show)
