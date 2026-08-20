module Pawl.Types.TopOfLibraryUntil where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | The top of a library (CR 401.2, CR 121.1) walked down to and INCLUDING the
-- first card the Filter matches, named by whose library and by what ends the
-- walk: Treasure Hunt's "reveal cards from the top of your library until you
-- reveal a nonland card".
--
-- A Filter where Pawl.Types.TopOfLibrary carries a Pawl.Types.Quantity, and that
-- is the only difference between them: both name a prefix of the pile from its
-- head, one measured and one terminated. The Filter is matched against each
-- card's own projection as the walk reaches it (CR 608.2c), so a library holding
-- no match gives up all of it (CR 609.3).
--
-- The fields are named rather than positional, the shape every other
-- Pawl.Types.ObjectRef payload record takes.
data TopOfLibraryUntil = MkTopOfLibraryUntil
  { player :: PlayerRef.PlayerRef,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
