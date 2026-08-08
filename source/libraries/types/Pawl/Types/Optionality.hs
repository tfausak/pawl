module Pawl.Types.Optionality where

-- | CR 603.5: whether a clause's instructions are OPTIONAL -- the printed "may".
-- The ability goes on the stack regardless, and the choice is made as it
-- resolves.
--
-- That timing is why this rides Pawl.Types.Clause and is not a
-- ModeSelection.ChooseUpTo over a one-mode payload. Modes and targets are chosen
-- as the spell is cast (CR 601.2b/601.2c, CR 700.2b, CR 603.3d); a "may" is
-- decided strictly later, while the effect is applied (CR 608.2d). As a mode
-- selection, an ability the player declines would leave the stack with no legal
-- mode instead of resolving and doing nothing.
--
-- Not a Bool, for the reason Regenerability and TapState are not: `Optional` says
-- which rule is in play where `True` would say nothing.
--
-- Scoped to a CLAUSE (CR 608.2e), which is the span one printed "may" governs.
-- A "may" over two instructions is one clause and so one question, as the
-- printed English says; two adjacent printed "may"s are two clauses and two
-- questions. Shed Weakness is the card that separates the two readings.
data Optionality
  = Mandatory
  | Optional
  deriving (Eq, Ord, Show)
