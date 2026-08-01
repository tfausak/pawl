module Pawl.Types.Mana where

import Pawl.Types.ManaUnit (ManaUnit)

-- A mana pool's contents (CR 106.4): a multiset of units, NOT counts per type.
--
-- Counts discard provenance by construction, and mana is not fungible -- snow
-- {S} and spend-restrictions care where a unit came from. Counts to units would
-- be a rewrite of every payment call site; units to richer-units is a field
-- addition. The bet has since been collected: CR 107.4h's {S} added
-- ManaUnit.tags and no call site here moved.
newtype Mana = MkMana [ManaUnit]
  deriving (Eq, Ord, Show)
