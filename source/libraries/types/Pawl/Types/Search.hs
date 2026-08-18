module Pawl.Types.Search where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SearchDestination as SearchDestination

-- | CR 701.23's search: who looks, whose library, how many cards, which cards,
-- and where they go.

-- The two PlayerRefs are the reason the fields are named. CR 701.23a's looking
-- and CR 400.1's ownership are independent -- Extract's "search target player's
-- library" says You/InSlot -- and both fields have the same type, so a
-- positional payload lets a card file swap the searcher and the owner and still
-- decode. Every other producer says the same ref twice, which is exactly the
-- shape under which such a swap goes unnoticed.
--
-- Independent does not mean unrelated. Two independent refs cross, so a card
-- whose searcher names the whole table -- Jungle Wayfinder's "each player may
-- search THEIR library" -- would have every player search every library, which
-- is a different and strictly larger instruction. The COUPLED reading is spelled
-- by an `owner` of PlayerRef.Candidate: CR 701.23a's one instruction applied per
-- player, with the library read being whichever searcher the resolution has
-- reached.
data Search = MkSearch
  { searcher :: PlayerRef.PlayerRef,
    owner :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity,
    filter :: Filter.Filter Keyword.Keyword,
    -- | Whether the printed instruction says "up to", making the quantity a
    -- ceiling the searcher chooses within rather than one they must fill.
    --
    -- Not derivable from `filter`. CR 701.23b already lets a search STATING A
    -- QUALITY find fewer or none, so Explosive Vegetation's "up to two basic
    -- land cards" needs no flag; CR 701.23d then forces a search for a bare
    -- quantity, Extract's "a card". Denying Wind's "up to seven cards" is the
    -- third case the other two fields cannot express: a bare quantity the card
    -- nonetheless makes optional, which CR 701.23d does not reach because the
    -- search is not "simply for a quantity of cards".
    --
    -- A Bool beside the Quantity rather than a Quantity arm: "up to" is a
    -- permission attached to a count, not a different count. An unbounded "any
    -- number of cards" is the other axis -- a count with no number at all --
    -- and belongs on the Quantity (gap #1685), where it would carry this flag
    -- too.
    upTo :: Bool,
    destination :: SearchDestination.SearchDestination
  }
  deriving (Eq, Ord, Show)
