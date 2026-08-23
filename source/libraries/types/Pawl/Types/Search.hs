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
-- by an `owner` of PlayerRef.Candidate: the library read is whichever searcher
-- the resolution has reached. CR 701.23a says only how to LOOK, so WHICH library
-- is the card's own sentence, and "their library" names the searcher's -- one
-- instruction applied per player rather than a fold of its own.
data Search = MkSearch
  { searcher :: PlayerRef.PlayerRef,
    owner :: PlayerRef.PlayerRef,
    -- | How many cards the search may find, or 'Nothing' where the printed
    -- instruction states no count at all -- Mana Severance's "any number of land
    -- cards".
    --
    -- A Maybe rather than a Quantity arm: CR 701.23a's find is bounded by what
    -- the zone holds, and "any number" is the ABSENCE of a count rather than a
    -- count that evaluates to something. Pawl.Engine.Quantity.evaluateFor
    -- already answers Nothing for a quantity it cannot evaluate, which the
    -- search turns into a cap of zero, so an unbounded arm there would be
    -- indistinguishable from an unevaluable one at every other call site.
    --
    -- Unbounded does not mean mandatory. CR 701.23b still governs a search
    -- stating a quality, so Mana Severance may find none of the lands it can see;
    -- CR 701.23d's "simply for a quantity of cards" cannot reach a search that
    -- states no quantity, so an unbounded search never completes its answer from
    -- the cards it passed over.
    quantity :: Maybe Quantity.Quantity,
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
    -- number of cards" is the other axis, and is spelled by the absence of the
    -- count itself -- over which this flag is unobservable, both readings
    -- landing in CR 701.23b's branch.
    upTo :: Bool,
    destination :: SearchDestination.SearchDestination
  }
  deriving (Eq, Ord, Show)
