module Pawl.Types.PerCreature where

import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Quantity as Quantity

-- | ONE taxed creature's share of a cost to attack (Pawl.Types.AttackCost) or of
-- a cost to block (Pawl.Types.BlockCost), which CR 508.1h and CR 509.1d
-- respectively then total over the whole declaration.
--
-- ONE type for both, and the sharing is the rules' rather than a convenience: the
-- two clauses are the same sentence about two turn-based actions, and Oppressive
-- Rays prints both halves in one line with one {3}. What differs between the two
-- carriers is WHICH declaration the share is repeated over, which is each
-- carrier's own business.
--
-- Two arms because the two families of printing differ in KIND and not in value.
-- Ghostly Prison's {2} and Norn's Annex's {W/P} are mana SYMBOLS, and a
-- Pawl.Types.ManaCost is a list of them precisely so a mixed or Phyrexian cost
-- stays sayable. Sphere of Safety's and Collective Restraint's {X} is not a
-- symbol at all until the board is read, so it is a Pawl.Types.Quantity.
--
-- Counted carries no colour, and that is the printings rather than a
-- simplification: every counted share in the pool is written {X}, and CR 107.4b
-- makes a variable symbol generic mana in a cost.
data PerCreature
  = Fixed ManaCost.ManaCost
  | -- | Cashed out as that many generic mana by Pawl.Engine.AttackCost.costsOn
    -- and Pawl.Engine.BlockCost.costsOn, LIVE at the moment CR 508.1h or CR
    -- 509.1d determines the total. The lock-in the rule then imposes belongs to
    -- Pawl.Engine.Combat.declareAttackers and declareBlockers, which bind the
    -- total once, exactly as they do for Fixed.
    Counted Quantity.Quantity
  deriving (Eq, Ord, Show)
