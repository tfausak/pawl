module Pawl.Types.AbilityAddsMana where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaSpecification as ManaSpecification
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 605.1b's "mana being added to a player's mana pool", read by a
-- bystander: which player has to add it, what the ability's source has to be,
-- and which mana they have to add -- Caged Sun's "whenever a land's ability
-- causes you to add one or more mana of the chosen color".
--
-- A record with no defaults, Pawl.Types.PermanentTappedForMana's shape and its
-- reason.
data AbilityAddsMana = MkAbilityAddsMana
  { player :: PlayerRelation.PlayerRelation,
    source :: Filter.Filter Keyword.Keyword,
    mana :: ManaSpecification.ManaSpecification
  }
  deriving (Eq, Ord, Show)
