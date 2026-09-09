module Pawl.Types.PermanentTappedForMana where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaSpecification as ManaSpecification
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 106.12a read by a bystander: which player's tap fires the ability, which
-- quality the permanent they tapped has to have, and which mana that tap had to
-- produce -- Autumn Willow, Harmony's "whenever you tap a land creature for
-- mana".
--
-- A record for Pawl.Types.PermanentSacrificed's reason: the printed form pairs a
-- subject with its narrowings, and no field has a default -- Mirari's Wake's
-- "whenever you tap a land for mana" spells the subject out as a Land filter
-- rather than leaving it absent, and its silence about the mana out as
-- ManaSpecification.AnyMana.
data PermanentTappedForMana = MkPermanentTappedForMana
  { player :: PlayerRelation.PlayerRelation,
    filter :: Filter.Filter Keyword.Keyword,
    -- | CR 106.12a's second half, "or is tapped for mana of a specified type" --
    -- Gauntlet of Power's "for mana of the chosen color". No default here
    -- either: a wording that narrows the mana not at all spells itself out as
    -- ManaSpecification.AnyMana, as one that narrows the subject not at all
    -- spells itself out as PlayerRelation.AnyPlayer.
    mana :: ManaSpecification.ManaSpecification
  }
  deriving (Eq, Ord, Show)
