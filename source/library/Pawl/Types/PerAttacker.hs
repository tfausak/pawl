module Pawl.Types.PerAttacker where

import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Quantity as Quantity

-- | ONE taxed attacker's share of a cost to attack (Pawl.Types.AttackCost),
-- which CR 508.1h then totals over the whole declaration.
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
data PerAttacker
  = Fixed ManaCost.ManaCost
  | -- | Cashed out as that many generic mana by
    -- Pawl.Engine.AttackCost.costsOn, LIVE at the moment CR 508.1h determines
    -- the total. The lock-in the rule then imposes belongs to
    -- Pawl.Engine.Combat.declareAttackers, which binds the total once, exactly
    -- as it does for Fixed.
    Counted Quantity.Quantity
  deriving (Eq, Ord, Show)
