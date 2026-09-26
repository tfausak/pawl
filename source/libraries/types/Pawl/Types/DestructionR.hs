module Pawl.Types.DestructionR where

import qualified Pawl.Types.DestructionRewrite as DestructionRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.ReplacementEffect's DestructionR arm: which
-- permanent's destruction is intercepted, and what happens instead.
data DestructionR = MkDestructionR
  { -- | CR 614.1a: the printed subject (Pyramids' "target land"). Nothing is
    -- the subject the rewrite's own rule names (Pawl.Engine.Replacement.scopes).
    matching :: Maybe (Filter.Filter Keyword.Keyword),
    rewrite :: DestructionRewrite.DestructionRewrite
  }
  deriving (Eq, Ord, Show)
