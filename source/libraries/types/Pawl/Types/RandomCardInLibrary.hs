module Pawl.Types.RandomCardInLibrary where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | Alchemy's seek over CR 401.2: whose library randomness reaches, what it may
-- name there, and how many cards it names.

-- Pawl.Types.RandomCardInHand's record one hidden zone over, and for that type's
-- reasons: ONE PlayerRef, since CR 401.2 hides a library from its owner as much
-- as from anyone and so nobody's choice stands between the zone and the pick;
-- the FILTER narrows the candidates rather than adding a roll (Gate to
-- Seatower's "seek a nonland card" is one pick among the nonland cards, not a
-- pick that may miss); and the COUNT names DISTINCT cards, with CR 609.3 covering
-- the shortfall.
--
-- Seek is digital-only, so Arena's text rather than the CR is its authority, and
-- that text states what separates it from CR 701.23's search: nobody looks at
-- the library, nothing is revealed and nothing is shuffled.
data RandomCardInLibrary = MkRandomCardInLibrary
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
