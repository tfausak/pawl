module Pawl.Type.Optionality where

-- CR 603.5: whether a mode's instructions are OPTIONAL -- the printed "may", as
-- in "At the beginning of your upkeep, you may draw a card". "These abilities go
-- on the stack when they trigger, regardless of whether their controller intends
-- to exercise the ability's option or not. The choice is made when the ability
-- resolves."
--
-- That last sentence is why this is a property of the MODE and not a
-- ModeSelection.ChooseUpTo over a one-mode payload. Modes are chosen as the
-- spell is cast or the ability is put on the stack (CR 601.2b / 700.2b), and so
-- are targets (CR 601.2c / 603.3d); a "may" is decided strictly later, while the
-- effect is being applied (CR 608.2d). Encoding it as a mode selection would
-- move the choice earlier and would make an ability the player declines leave
-- the stack with no legal mode instead of resolving and doing nothing.
--
-- Not a Bool, for the reason Regenerability and TapState are not Bools: at a
-- read site `Optional` says which rule is in play where `True` would say
-- nothing at all.
--
-- Scoped to a whole MODE -- the mode's effect list is the unit the one "may"
-- covers, so a card whose "may" spans two instructions ("you may draw a card and
-- lose 1 life") is one question rather than two, which is what the printed
-- English says. A card whose "may" covers only SOME of its instructions ("Draw a
-- card. You may discard a card.") is the case this cannot express, and needs a
-- per-instruction wrapper (#335).
data Optionality
  = Mandatory
  | Optional
  deriving (Eq, Ord, Show)
