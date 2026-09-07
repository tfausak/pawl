module Pawl.Types.BuybackDecision where

-- | CR 702.27a: a player's answer to buyback's offer of an additional cost as
-- they cast a spell. What hangs off it is that rule's second static ability --
-- "if the buyback cost was paid, put this spell into its owner's hand instead of
-- into that player's graveyard as it resolves" -- so this is the announcement CR
-- 601.2b asks for and not a report of a payment that has happened.
--
-- A named sum rather than a Bool, the posture every player-facing yes-or-no in
-- this engine takes, so a transcript reads as the decision it records.
--
-- Its own type rather than a reuse of Pawl.Types.EntwineDecision, whose rule
-- bundles a MODE choice into the same sentence, of Pawl.Types.KickerDecision,
-- which is a COUNT because rule 702.33c's multikicker offers its cost any number
-- of times, or of OptionalDecision, which is scoped to CR 603.5's printed "may"
-- answered AS THE SPELL RESOLVES. Rule 702.27a states one cost, payable once,
-- announced while the spell is being CAST.
data BuybackDecision
  = Declines
  | BuysBack
  deriving (Eq, Ord, Show)
