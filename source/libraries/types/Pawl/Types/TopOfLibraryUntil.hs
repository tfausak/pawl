module Pawl.Types.TopOfLibraryUntil where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | The top of a library (CR 401.2, CR 121.1) walked down to and INCLUDING the
-- card whose match ENDS the walk, named by whose library, by what a match is,
-- and by how many matches it takes: Treasure Hunt's "reveal cards from the top
-- of your library until you reveal a nonland card" is one match, Open the Way's
-- "until you reveal X land cards" is X of them.
--
-- Pawl.Types.TopOfLibrary carries the same `player` and `count` and no Filter,
-- and that Filter is the whole difference between them: both name a prefix of
-- the pile from its head, one measured in CARDS and one in MATCHES. The Filter is
-- matched against each card's own projection as the walk reaches it (CR 608.2c),
-- so a library holding fewer matches than the count gives up all of it (CR
-- 609.3).
--
-- The count is a Quantity for TopOfLibrary's reason: Open the Way's is CR
-- 601.2b's announced X, read when the effect executes (CR 608.2c), and an
-- unevaluable or negative one is clamped to zero there.
--
-- The fields are named rather than positional, the shape every other
-- Pawl.Types.ObjectRef payload record takes.
data TopOfLibraryUntil = MkTopOfLibraryUntil
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
