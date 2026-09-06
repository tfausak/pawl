module Pawl.Types.PermanentTappedForMana where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 106.12a read by a bystander: which player's tap fires the ability, and
-- which quality the permanent they tapped has to have -- Autumn Willow,
-- Harmony's "whenever you tap a land creature for mana".
--
-- A record for Pawl.Types.PermanentSacrificed's reason: the printed form pairs a
-- subject with a narrowing, and neither half has a default -- Mirari's Wake's
-- "whenever you tap a land for mana" spells the narrowing out as a Land filter
-- rather than leaving it absent.
data PermanentTappedForMana = MkPermanentTappedForMana
  { player :: PlayerRelation.PlayerRelation,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
