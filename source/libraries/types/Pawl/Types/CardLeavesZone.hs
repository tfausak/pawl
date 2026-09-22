module Pawl.Types.CardLeavesZone where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnScope as TurnScope
import qualified Pawl.Types.Zone as Zone

-- | Which departing cards fire a "card leaves a zone" ability, from and to
-- where, and whose turns count -- Kishla Skimmer's "whenever a card leaves your
-- graveyard during your turn", Rakshasa Vizier's "one or more cards are put into
-- exile from your graveyard".
--
-- Shared by both of CR 603.2c's readings: TriggerCondition's CardLeavesZone
-- carries it per card and CardsLeaveZone once per batch (Spirit Mascot),
-- and nothing in this record differs between them.
--
-- A record for Pawl.Types.SpellCast's reason, and with the same two halves: the
-- Filter is read against the card that LEFT (CR 608.2h), so "your graveyard" is
-- a Filter.OwnedBy conjunct -- CR 400.3 puts a card in its owner's graveyard,
-- and Pawl.Types.Zone names no player -- while the turn is no characteristic of
-- any object and comes from the game state instead.
data CardLeavesZone = MkCardLeavesZone
  { filter :: Filter.Filter Keyword.Keyword,
    scope :: TurnScope.TurnScope,
    -- | The zone the card left (CR 400.7).
    from :: Zone.Zone,
    -- | The zone it went to, where the printing names one; Nothing admits any.
    to :: Maybe Zone.Zone
  }
  deriving (Eq, Ord, Show)
