module Pawl.Types.ModifiedRoll where

import qualified Numeric.Natural as Natural
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
-- decrease, so nothing has moved the number when this is asked.
--
-- An exact number rather than a Pawl.Types.Comparison, because that is what the
-- one printing says; a card gating on a range would widen this field rather than
-- add one.
--
-- Not implemented: a modifier that is MANDATORY, one carrying a cost, and one
-- reaching a roll another player made (#3981). CR 706.2a allows the first two,
-- and every printed reroll is a bare "may", so the engine asks and charges
-- nothing; the seat is Pawl.Types.PlayerStaticAbility's PlayerScope, which the
-- one printing writes as "you".
--
-- Construct with BRACE syntax: `sides` and `natural` are two Naturals in a row,
-- and positional construction that transposed them would compile.
data ModifiedRoll = MkModifiedRoll
  { sides :: Maybe Natural.Natural,
    natural :: Maybe Natural.Natural,
    modifier :: RollModifier.RollModifier
  }
  deriving (Eq, Ord, Show)
