module Pawl.Types.CostScale where

import qualified Pawl.Types.Color as Color

-- | CR 601.2f: how many times an effect's added components join the cost it is
-- adjusting. Rides Pawl.Types.AddActivationCost and Pawl.Types.AddSpellCost,
-- and is cashed by Pawl.Engine.Cost.plusComponents against the cost CR 601.2b
-- announced -- before CR 601.2f's reductions, which is the ordering that rule
-- states.
--
-- Not a Natural multiplier and not a Pawl.Types.Quantity: a Quantity is
-- evaluated against the BOARD (Pawl.Engine.Quantity.evaluateFor takes a game
-- state), and this one is a function of the COST being adjusted, which no
-- Quantity is ever handed. Not a Bool, for Pawl.Types.Optionality's reason --
-- @PerColoredSymbol@ says which rule is in play where @True@ would say only
-- that something is different.
data CostScale
  = -- | Once, whatever the cost looks like: Brutal Suppression's "cost an
    -- additional \"Sacrifice a land\" to activate", with no "for each".
    Once
  | -- | Once per mana symbol of this colour in the cost being adjusted:
    -- Drought's "for each black mana symbol in their mana costs". What counts is
    -- CR 107.4a's coloured mana symbol, which CR 107.4e's hybrid and CR 107.4f's
    -- Phyrexian symbols also are -- Drought's 2008-08-01 ruling states the
    -- hybrid half ("A hybrid symbol that is both black and another type is a
    -- black mana symbol, regardless of what cost is paid for it") and rule
    -- 107.4f is the Phyrexian half, which postdates it.
    -- Pawl.Engine.Projection.symbolColors is the shared classifier that makes
    -- all three fall out.
    PerColoredSymbol Color.Color
  deriving (Eq, Ord, Show)
