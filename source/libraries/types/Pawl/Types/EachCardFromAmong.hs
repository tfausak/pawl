module Pawl.Types.EachCardFromAmong where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.SlotName as SlotName

-- | The printed words "all land cards revealed this way": EVERY member of a
-- group an earlier effect of this resolution bound that the Filter matches --
-- Mulch's "put all land cards revealed this way into your hand".
--
-- Pawl.Types.ChosenCardFromAmong's plural, and the same two fields for the same
-- reasons: a SLOT rather than a zone, because the candidates are the cards an
-- earlier effect named (CR 701.20a's reveal, CR 701.20e's look) and where those
-- cards are is wherever that effect left them -- which is how a batch still in a
-- LIBRARY is reachable at all, a library having no filtered sweep (#1309).
--
-- The whole difference from that type is that nobody is ASKED. A count-free
-- "all" is not a choice, so CR 608.2d has nothing to hand out; the members that
-- match are named by their own characteristics, read against the CR 613
-- projection when the instruction is reached (CR 608.2c). Which is also why this
-- arm needs no note about who may choose (#1957) and no note about a count above
-- one (#1956) -- it takes every match, and CR 609.3 covers a group holding none.
--
-- "The rest" needs no arm of its own: a LATER clause reading the same slot with
-- Pawl.Types.ObjectRef.InSlot finds the matched cards gone, CR 400.7 having
-- minted new objects for them on the way to their new zone. That is the split
-- Mulch's one sentence makes -- the matching half here, the remainder through
-- InSlot -- and it is the reading PR #1958 established for
-- ChosenCardFromAmong's singular.
--
-- The fields are named rather than positional, the shape every other
-- Pawl.Types.ObjectRef payload record takes.
data EachCardFromAmong = MkEachCardFromAmong
  { slot :: SlotName.SlotName,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
