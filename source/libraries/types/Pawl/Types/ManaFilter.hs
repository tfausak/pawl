module Pawl.Types.ManaFilter where

import qualified Pawl.Types.ManaType as ManaType

-- | Which of the mana in a pool a card's text is about -- Upwelling's "unspent
-- mana" against Omnath, Locus of Mana's "unspent green mana".
--
-- A SEPARATE, tiny type rather than a use of Pawl.Types.Filter. Every Filter atom
-- is a characteristic of an object (CR 109.3) or an identity of a player, and a
-- Pawl.Types.ManaUnit has neither. This type exists to avoid widening Filter with
-- atoms meaningless for every other candidate it serves.
--
-- Arms rather than one Set of the six CR 106.1b types: Upwelling prints "unspent
-- mana" with no type named, and spelling that as an enumeration would put the
-- closed half's list of mana types into open-half card data.
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
  | -- | The complement of OfType: every unit that is not of this type.
    -- Celestial Dawn's "other mana", which its own sentence defines by
    -- exclusion from the white its first clause spoke about.
    --
    -- A COMPLEMENT OF ONE TYPE and not a general `Not ManaFilter`. Every
    -- printed "other mana" is worded against a single named type, so recursion
    -- would buy nothing this type's two readers
    -- (Pawl.Engine.ManaFilter.matchesType is the only interpreter) can use, and
    -- CR 106.1b's closed list means the complement is always writable the long
    -- way.
    NotOfType ManaType.ManaType
  deriving (Eq, Ord, Show)
