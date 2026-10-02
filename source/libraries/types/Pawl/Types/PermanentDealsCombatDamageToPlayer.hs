module Pawl.Types.PermanentDealsCombatDamageToPlayer where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 603.2 / 510.2: which permanent's combat damage fires the ability, and
-- which player it has to have been dealt to -- Teysa, Envoy of Ghosts'
-- "whenever a creature deals combat damage to you".
--
-- Not Pawl.Types.PermanentsDealCombatDamageToPlayer: that record's
-- `oneOrMorePlayers` has no singular reading.
data PermanentDealsCombatDamageToPlayer = MkPermanentDealsCombatDamageToPlayer
  { filter :: Filter.Filter Keyword.Keyword,
    recipient :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
