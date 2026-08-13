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
data Search = MkSearch
  { searcher :: PlayerRef.PlayerRef,
    owner :: PlayerRef.PlayerRef,
    quantity :: Quantity.Quantity,
    filter :: Filter.Filter Keyword.Keyword,
    destination :: SearchDestination.SearchDestination
  }
  deriving (Eq, Ord, Show)
