module Pawl.Types.TopOfLibrary where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | The top cards of a library, named by whose library and how many. CR 401.2
-- makes a library an ordered pile and CR 121.1 names its head the top, which
-- together are what a POSITION in one means.
--
-- The depth is a Quantity rather than a Natural, so Commune with Lava's "exile
-- the top X cards of your library" is sayable beside Act on Impulse's literal
-- three. A computed depth is read when the effect executes (CR 608.2c), and
-- Pawl.Engine.Resolve.objectRefObjects clamps an unevaluable or negative answer
-- to zero (CR 107.1b).
--
-- A depth that a FILTER ends instead of a number is
-- Pawl.Types.TopOfLibraryUntil.
data TopOfLibrary = MkTopOfLibrary
  { player :: PlayerRef.PlayerRef,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
