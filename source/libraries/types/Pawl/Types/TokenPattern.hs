module Pawl.Types.TokenPattern where

import qualified Pawl.Types.ControllerRelation as ControllerRelation
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | CR 111.1 / 614.1: which token creations a token replacement intercepts.
-- Doubling Season's token clause is Yours ("under your control") over any
-- token; Queen Allenal of Ruadach's narrows `whatToken` to creature tokens.
data TokenPattern = MkTokenPattern
  { whose :: ControllerRelation.ControllerRelation,
    -- | What the token being created IS, judged off its characteristics as it
    -- would exist (CR 614.12). A creation of several lots matches when any
    -- lot does.
    whatToken :: Filter.Filter Keyword.Keyword
  }
  deriving (Eq, Ord, Show)
