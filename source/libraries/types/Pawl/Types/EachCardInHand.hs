module Pawl.Types.EachCardInHand where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.Keyword as Keyword

-- | CR 400.1: every card in the named players' hands that matches -- Amnesia's
-- "target player reveals their hand and discards all nonland cards".
--
-- Pawl.Types.EachCardInGraveyard's shape over the other per-player zone, and
-- resolved the same way: the same scope, read by the same
-- Pawl.Engine.Target.graveyardScopePlayers, and the same fold in
-- Pawl.Engine.Resolve.objectRefObjects. The zone is BAKED IN rather than carried
-- as a Pawl.Types.Zone, for the reason that arm's header gives.
--
-- THE HIDDEN-ZONE QUESTION (CR 400.2) IS THE CARD'S TO ANSWER, NOT THE ENGINE'S.
-- Pawl.Types.ObjectRef.EachCardInYourHand avoids it by reaching only the
-- resolving controller's own hand; this arm reaches anyone's, so which cards
-- matched is read off what left the zone. pawl models CR 400.2 as a rules
-- property -- which pools and slots may name a hidden zone -- rather than as
-- information hiding in Pawl.Types.GameState, so what makes this arm legitimate
-- is that its producer PRINTS the reveal (CR 701.20a) that shows the hand first.
-- A card sweeping a hand it does not reveal would leak, and none is written.
--
-- The Filter is OPTIONAL, Pawl.Types.ObjectRef.EachCardExiledWithSource's
-- reason: Pawl.Types.Filter has no tautological arm, so "reveals their hand" --
-- the whole of it, which is Amnesia's first instruction -- has no other spelling.
--
-- The scope is a Pawl.Types.GraveyardScope, whose name reads of the wrong zone
-- (#2074): the type is CR 400.1's "whose", stated once for every per-player zone
-- and for a target pool as much as for a sweep, and its two arms carry no
-- graveyard in them.
--
-- Not a target and never one (CR 115.10a), so CR 608.2b has nothing to fizzle,
-- and swept when the effect executes (CR 608.2c) -- the two properties every
-- sweeping arm has.
data EachCardInHand = MkEachCardInHand
  { hands :: GraveyardScope.GraveyardScope,
    filter :: Maybe (Filter.Filter Keyword.Keyword)
  }
  deriving (Eq, Ord, Show)
