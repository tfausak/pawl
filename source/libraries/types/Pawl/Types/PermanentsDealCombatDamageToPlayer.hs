module Pawl.Types.PermanentsDealCombatDamageToPlayer where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 603.2c / 510.2: which permanents' combat damage fires the batch
-- ability, and which player it has to have been dealt to -- Norn's Decree's
-- "one or more creatures an opponent controls deal combat damage to you", Pia
-- Nalaar, Chief Mechanic's "... you control deal combat damage to a player".
--
-- A record for Pawl.Types.PlayerAttacksWith's reason: the printed form names
-- two subjects, the damagers and the damaged player, and the two are read from
-- the same CR 109.5 perspective.
data PermanentsDealCombatDamageToPlayer = MkPermanentsDealCombatDamageToPlayer
  { filter :: Filter.Filter Keyword.Keyword,
    recipient :: PlayerRelation.PlayerRelation,
    -- | CR 603.2c: "to ONE OR MORE PLAYERS" (Forth Eorlingas!), one occurrence
    -- however many players the step damaged, where False is "to a player", one
    -- occurrence per damaged player.
    oneOrMorePlayers :: Bool
  }
  deriving (Eq, Ord, Show)
