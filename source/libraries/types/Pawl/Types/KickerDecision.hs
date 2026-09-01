module Pawl.Types.KickerDecision where

import qualified Numeric.Natural as Natural

-- | CR 702.33a / 702.33c: a player's answer to one kicker cost's offer as they
-- cast a spell -- how many times they declare they will pay it. CR 702.33d is
-- what hangs off the answer -- "if a spell's controller declares the intention to
-- pay any of that spell's kicker costs, that spell has been 'kicked'" -- so this
-- is the declaration itself and not a report of a payment that has happened.
--
-- A COUNT and not a yes-or-no, because rule 702.33c's multikicker offers the same
-- cost "any number of times" and rule 702.33c's last sentence makes that cost a
-- kicker cost like any other. Zero is the decline every kicker offer admits; a
-- CR 702.33a kicker is asked with a limit of one (Pawl.Types.Prompt's
-- ChooseKicker), and an answer past the limit rejects the cast rather than being
-- clamped, which is Prompt.ChooseX's posture (Pawl.Engine.Cast).
--
-- Its own type rather than a reuse of Pawl.Types.EntwineDecision, whose rule
-- bundles a MODE choice into the same sentence, or of OptionalDecision, which is
-- scoped to CR 603.5's printed "may" answered AS THE SPELL RESOLVES. This one is
-- answered while the spell is being CAST (CR 601.2b) and widens nothing.
newtype KickerDecision = MkKickerDecision
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
