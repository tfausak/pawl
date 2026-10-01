module Pawl.Types.TokenR where

import qualified Pawl.Types.Scaling as Scaling
import qualified Pawl.Types.TokenPattern as TokenPattern
import qualified Pawl.Types.TokenPlus as TokenPlus

-- | The payload of Pawl.Types.ReplacementEffect's TokenR arm (#1305): which
-- token creations are intercepted, how the number created is scaled, and what
-- is created alongside them.
--
-- Parametric in @card@ for Pawl.Types.Create's reason: the appended token's
-- card is card DATA nested inside card data.
data TokenR card = MkTokenR
  { matching :: TokenPattern.TokenPattern,
    -- | Doubling Season's "twice that many". Nothing leaves the count alone.
    scaling :: Maybe Scaling.Scaling,
    -- | CR 614.1a's "those tokens plus ... are created instead": a lot appended
    -- to the SAME creation event, so a later CR 616.1 row scales it along with
    -- the rest, and the riders of the creating effect reach it.
    plus :: Maybe (TokenPlus.TokenPlus card)
  }
  deriving (Eq, Ord, Show)
