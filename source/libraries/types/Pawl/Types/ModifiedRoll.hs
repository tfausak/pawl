module Pawl.Types.ModifiedRoll where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PermissionLimit as PermissionLimit
import qualified Pawl.Types.RollModifier as RollModifier

-- | The payload of Pawl.Types.PlayerEffect's ModifyDieRoll arm (CR 706.2's third
-- sentence): a modifier this player's die rolls take from a source OTHER than
-- the instruction that ordered them -- which rolls it watches, and what it does
-- to the natural result.
--
-- The player-axis sibling of Pawl.Types.StatedFlip one rule over, and there for
-- its reason: rule 706.2's "other sources" reach a roll nobody targeted and no
-- instruction named, which is a continuous effect over a PLAYER rather than
-- anything a resolution could carry. NOT a Pawl.Types.ReplacementEffect: rule
-- 706.2 never says "instead", and CR 614.1a's loop runs before a die has come
-- up at all, where every modifier here arrives after one has.
--
-- `sides` is the die the modifier states, and Nothing states none -- Clam-I-Am's
-- "on a six-sided die" against Wall of Fortune's bare "a die". CR 706.1a makes
-- the size the whole description of a die, so this is the only narrowing a card
-- has written.
--
-- `natural` is the number the modifier states the die came up, and Nothing
-- states none: Clam-I-Am's "if you roll a 3". CR 706.2b is what makes reading
-- the NATURAL result right -- rerolls are considered before any increase or
-- decrease, so nothing has moved the number when this is asked. Read by a
-- Reroll alone; no increase-or-decrease printing states one.
--
-- An exact number rather than a Pawl.Types.Comparison, because that is what the
-- one printing says; a card gating on a range would widen this field rather than
-- add one.
--
-- `cost` is CR 706.2a's "associated cost", and Nothing states none -- Clam-I-Am's
-- bare "you may reroll" against Wall of Fortune's "you may tap an untapped Wall
-- you control to". Paid by the player the MODIFIER's carrier names as "you" (CR
-- 109.5), who is not always the roller: Wall of Fortune's scope is every player
-- and its "you" is the Wall's controller.
--
-- `limit` is the printed budget on taking the modifier at all, Night Shift of
-- the Living Dead's "Do this only once each turn", spent only when it is taken;
-- Unlimited where the card states none. Read by an IncreaseOrDecrease alone: no
-- reroll printing states a budget.
--
-- Not implemented: a modifier that is MANDATORY (#3981). CR 706.2a allows one
-- and no printing states one: Scryfall @o:reroll@, 2026-09-22, returns seven
-- printings and @o:reroll is:digital@ none, and every one of the seven is a bare
-- "may" (Clam-I-Am, Wall of Fortune, Monitor Monitor, Centaur of Attention) or
-- an activated ability whose cost is the ABILITY's rather than the modifier's
-- (Goblin Bookie, Pippa Duchess of Dice: ActivationRestriction.DuringDieRoll).
--
-- A STOP is what it would need before it needs a producer, and rule 706 states
-- none. The offer is re-read against each new natural result, so the decline is
-- what ends Pawl.Engine.Resolve.Effect's `rerolling`; take it away from a
-- modifier stating neither `sides` nor `natural` and the reroll matches its own
-- output forever, with real randomness as much as with a fixed answerer. The one
-- printed mandatory reroll is bounded by the card's own words and belongs to the
-- rolling instruction rather than to this type -- Ricochet's "reroll to break
-- ties, if necessary", see #3990.
--
-- Construct with BRACE syntax: `sides` and `natural` are two Naturals in a row,
-- and positional construction that transposed them would compile.
data ModifiedRoll = MkModifiedRoll
  { sides :: Maybe Natural.Natural,
    natural :: Maybe Natural.Natural,
    modifier :: RollModifier.RollModifier,
    cost :: Maybe (Cost.Cost Keyword.Keyword),
    limit :: PermissionLimit.PermissionLimit
  }
  deriving (Eq, Ord, Show)
