module Pawl.Types.EachCardInGraveyard where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 404.1: every card in the named players' graveyards that matches.
data EachCardInGraveyard = MkEachCardInGraveyard
  { players :: PlayerScope.PlayerScope,
    filter :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
