module Pawl.Types.TokenR where

import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.TokenPattern as TokenPattern

-- | The payload of Pawl.Types.ReplacementEffect's TokenR arm (#1305): which
-- token creations are intercepted, and how the number created is scaled.
data TokenR = MkTokenR
  { matching :: TokenPattern.TokenPattern,
    scaling :: Scaling.Scaling
  }
  deriving (Eq, Ord, Show)
