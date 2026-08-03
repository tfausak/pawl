module Pawl.Types.ManaFilter where

import qualified Pawl.Types.ManaType as ManaType

-- | Which of the mana in a pool a card's text is about -- Upwelling's "unspent
-- mana" against Omnath, Locus of Mana's "unspent green mana".
--
-- A SEPARATE, tiny type rather than a use of Pawl.Types.Filter, which is the
-- predicate every other candidate in the open half is kept by. Every Filter atom
-- is a characteristic of an object (CR 109.3) or an identity of a player, and a
-- Pawl.Types.ManaUnit has neither: no ObjectId, no characteristics, and
-- deliberately no source reference (its haddock says why one would dangle by
-- construction). Widening Filter with atoms that are meaningless for every other
-- candidate it serves is what this type exists to avoid.
--
-- The two arms, and not one Set of the six CR 106.1b types: "unspent mana" with
-- no type named is what Upwelling prints, and spelling it as an enumeration
-- would put the closed half's list of mana types into open-half card data, where
-- it would have to be re-authored if that list ever moved.
--
-- Not yet a predicate over a unit's Pawl.Types.ProductionTag -- {S}'s "mana
-- produced by a snow source" (CR 107.4h) is that shape, and no card in the pool
-- names it here (#252 is the neighbouring spending-restriction gap).
data ManaFilter
  = -- | Every unit in the pool, whatever its type. Upwelling.
    Any
  | -- | CR 106.1a / 106.1b: the units of exactly this type. Omnath, Locus of
    -- Mana's green.
    OfType ManaType.ManaType
  deriving (Eq, Ord, Show)
