module Pawl.Types.Optionality where

-- | CR 603.5: whether a mode's instructions are OPTIONAL -- the printed "may".
-- The ability goes on the stack regardless, and the choice is made as it
-- resolves.
--
-- That timing is why this is a property of the MODE and not a
-- ModeSelection.ChooseUpTo over a one-mode payload. Modes and targets are chosen
-- as the spell is cast (CR 601.2b/601.2c, CR 700.2b, CR 603.3d); a "may" is
-- decided strictly later, while the effect is applied (CR 608.2d). As a mode
-- selection, an ability the player declines would leave the stack with no legal
-- mode instead of resolving and doing nothing.
--
-- Not a Bool, for the reason Regenerability and TapState are not: `Optional` says
-- which rule is in play where `True` would say nothing.
--
-- Scoped to a whole MODE, so a "may" spanning two instructions is one question,
-- as the printed English says. A "may" covering only SOME of a mode's
-- instructions is the case this cannot express (#335).
data Optionality
  = Mandatory
  | Optional
  deriving (Eq, Ord, Show)
