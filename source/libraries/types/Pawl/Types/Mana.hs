module Pawl.Types.Mana where

import Pawl.Types.ManaUnit (ManaUnit)

-- A mana pool's contents (CR 106.4): a multiset of units, NOT counts per type.
--
-- Counts discard provenance by construction, and mana is not fungible -- snow
-- {S} and spend-restrictions care where a unit came from. Counts to units would
-- be a rewrite of every payment call site; units to richer-units is a field
-- addition. M1a reads nothing but manaType, so nothing is gained today; the
-- point is that nothing must be redone.
newtype Mana = MkMana [ManaUnit]
  deriving (Eq, Ord, Show)
