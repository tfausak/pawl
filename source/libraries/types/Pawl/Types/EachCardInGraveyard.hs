module Pawl.Types.EachCardInGraveyard where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.Keyword as Keyword

-- | CR 404.1: every card in the named players' graveyards that matches.
--
-- A Pawl.Types.GraveyardScope and not the Pawl.Types.PlayerScope this used to
-- carry, so the sweep can say Angel of Finality's "target player's graveyard" --
-- the players another slot of the same announcement holds. CR 400.1's per-player
-- zone leaves a sweep the same two readings a target pool has
-- (Pawl.Types.Pool.CardsInGraveyard), and both are answerable HERE because a
-- sweep resolves: Pawl.Engine.Resolve reads the InSlot arm off the bindings CR
-- 608.2b left legal, exactly as Pawl.Engine.Target reads it off the ones CR
-- 601.2c is choosing among.
data EachCardInGraveyard = MkEachCardInGraveyard
  { graveyards :: GraveyardScope.GraveyardScope,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
