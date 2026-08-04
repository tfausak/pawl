module Pawl.Types.Mana where

import qualified Pawl.Types.ManaUnit as ManaUnit

-- | A mana pool's contents (CR 106.4): a multiset of units, NOT counts per type.
--
-- Counts discard provenance by construction, and mana is not fungible -- snow
-- {S} (CR 107.4h) and spend restrictions care where a unit came from. Counts to
-- units would be a rewrite of every payment call site; units to richer units is a
-- field addition.
newtype Mana = MkMana
  { unwrap :: [ManaUnit.ManaUnit]
  }
  deriving (Eq, Ord, Show)
