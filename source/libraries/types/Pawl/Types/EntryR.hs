module Pawl.Types.EntryR where

import qualified Pawl.Types.EntryRewrite as EntryRewrite
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword

-- | The payload of Pawl.Types.ReplacementEffect's EntryR arm (#1305): which
-- objects entering the battlefield are intercepted, and how their entry is
-- rewritten.
data EntryR = MkEntryR
  { matching :: Filter.Filter Keyword.Keyword,
    rewrite :: EntryRewrite.EntryRewrite
  }
  deriving (Eq, Ord, Show)
