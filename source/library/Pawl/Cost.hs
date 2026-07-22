-- CR 601.2f: what a spell's total cost IS. Pawl.Mana keeps pools, production and
-- payment; this module keeps the cost itself, and is where P8's additional and
-- alternative costs land.
module Pawl.Cost where

import Numeric.Natural (Natural)
import qualified Pawl.PlayerEffect as PlayerEffect
import Pawl.Type.GameState (GameState)
import Pawl.Type.ManaCost (ManaCost)
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 601.2f: "The total cost is the mana cost or alternative cost (as determined
-- in rule 601.2b), plus all additional costs and cost increases, and minus all
-- cost reductions." `cost` arrives with X already substituted, because CR 601.2b
-- precedes 601.2f -- Sapphire Medallion's own ruling says so: "If a spell you cast
-- has {X} in its mana cost, you choose the value of X before calculating the
-- spell's total cost."
--
-- Additional costs are NOT here, so this is the total only in the sense of CR
-- 601.2f's increases and reductions: a spell's additional and alternative costs
-- are unmodelled (#4), and an activated ability's total cost never reaches this
-- function at all -- Pawl.Activate hands AbilityCost.mana straight to Pawl.Mana
-- (#90). Nor is the result ever "locked in": CR 601.2f's own last sentence makes
-- the total cost fixed once determined, but this function is recomputed fresh
-- from the current game state on every call, with no stored announcement record
-- (#94).
total :: PlayerId -> ObjectId -> ManaCost -> GameState -> ManaCost
total pid oid cost gs = applyAdjustments (PlayerEffect.costAdjustments pid oid gs) cost

-- The arithmetic half, pure and board-free.
--
-- 1. Every INCREASE is added to the generic component (CR 601.2f's order, and
--    Thalia's own ruling: "add any cost increases, then apply any cost
--    reductions").
-- 2. Every REDUCTION comes off the generic component ONLY (CR 118.7a: "Effects
--    that reduce a cost by an amount of generic mana affect only the generic
--    mana component of that cost. They can't affect the colored or colorless
--    mana components."), floored at zero -- a reduction with no generic left to
--    take is simply lost.
-- 3. CR 601.2f's "if the mana component of the total cost is reduced to nothing
--    ... it is considered to be {0}. It can't be reduced to less than {0}" needs
--    no special case: ManaCost is a list of symbols and the empty list IS {0}.
--
-- Reductions are SUMMED rather than applied one at a time. CR 601.2f's "if
-- multiple cost reductions apply, the player may apply them in any order" is a
-- prompt in the rules and an elision here (#88): every reduction P7 can express
-- is an amount of generic mana routed to the same component by CR 118.7a, so
-- summing is not merely equivalent to some order -- it is equivalent to EVERY
-- order.
--
-- The result is CANONICAL: one leading Generic symbol carrying the whole generic
-- component (omitted entirely when it is zero), then the printed typed symbols in
-- their original order. Presentation, not semantics -- Mana.spend sums every
-- generic symbol and matches typed symbols first -- but it is what makes a total
-- cost comparable, so "{U} taxed and then discounted is exactly {U}" is a
-- statement a test can make.
applyAdjustments :: ([Natural], [Natural]) -> ManaCost -> ManaCost
applyAdjustments adjustments cost =
  let (increases, reductions) = adjustments
      ManaCost.MkManaCost symbols = cost
      genericOf symbol = case symbol of
        ManaSymbol.Generic n -> n
        ManaSymbol.OfType _ -> 0
        -- Unreachable: CR 601.2b precedes 601.2f, so Mana.substituteX has
        -- already replaced every Variable before a total cost is computed. The
        -- match must be total, so a bare {X} contributes 0 generic.
        ManaSymbol.Variable -> 0
      isTyped symbol = case symbol of
        ManaSymbol.Generic _ -> False
        ManaSymbol.OfType _ -> True
        -- Unreachable for the same reason genericOf's Variable arm is: kept
        -- (retained, not stripped) so that if it ever were reachable, a bare
        -- {X} would still be treated as typed and survive the filter below.
        ManaSymbol.Variable -> True
      raised = sum (map genericOf symbols) + sum increases
      taken = sum reductions
      -- Natural subtraction is PARTIAL (it throws on underflow), so the CR
      -- 601.2f floor is also what keeps this total.
      lowered = if raised >= taken then raised - taken else 0
      leading = if lowered == 0 then [] else [ManaSymbol.Generic lowered]
   in ManaCost.MkManaCost (leading ++ filter isTyped symbols)
