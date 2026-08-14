module Pawl.Types.TurnUpR where

import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.TurnUpRewrite as TurnUpRewrite

-- | The payload of Pawl.Types.ReplacementEffect's TurnUpR arm (#1305): which
-- permanents being turned face up are intercepted (CR 614.1e), and how the
-- turning-over is rewritten.
data TurnUpR = MkTurnUpR
  { matching :: Filter.Filter Keyword.Keyword,
    rewrite :: TurnUpRewrite.TurnUpRewrite
  }
  deriving (Eq, Ord, Show)
