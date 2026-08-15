module Pawl.Types.PrintingId where

import qualified Numeric.Natural as Natural

-- | Which printing, within one game -- an index into GameState.printings.
--
-- Minted only by Pawl.Engine.Game.intern, so an id naming nothing is
-- unconstructible. That is why the table is game-local rather than a reference
-- into the registry: the pool cannot cover a token or an emblem, whose
-- characteristics are effect-defined (CR 111.3 / 114.3) and which are not cards
-- at all (CR 111.6), so a registry-keyed reference would dangle every time an
-- effect minted one.
newtype PrintingId = MkPrintingId
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
