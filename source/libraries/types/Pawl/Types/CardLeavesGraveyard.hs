module Pawl.Types.CardLeavesGraveyard where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnScope as TurnScope

-- | CR 603.10a's third look-back family: which departing cards fire the ability
-- and whose turns count -- Kishla Skimmer's "whenever a card leaves your
-- graveyard during your turn".
--
-- A record for Pawl.Types.SpellCast's reason, and with the same two halves: the
-- Filter is read against the card that LEFT (CR 608.2h), so "your graveyard" is
-- a Filter.OwnedBy conjunct -- CR 400.3 puts a card in its owner's graveyard,
-- and Pawl.Types.Zone names no player -- while the turn is no characteristic of
-- any object and comes from the game state instead.
data CardLeavesGraveyard = MkCardLeavesGraveyard
  { filter :: Filter.Filter Keyword.Keyword,
    scope :: TurnScope.TurnScope
  }
  deriving (Eq, Ord, Show)
