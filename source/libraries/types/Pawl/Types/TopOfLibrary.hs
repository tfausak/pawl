module Pawl.Types.TopOfLibrary where

import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity

-- | CR 401.1: the top cards of a library, named by whose library and how many.
--
-- The depth is a Quantity rather than a Natural, so Commune with Lava's "exile
-- the top X cards of your library" is sayable beside Act on Impulse's literal
-- three. A computed depth is read when the effect executes (CR 608.2c), and
-- Pawl.Engine.Resolve.objectRefObjects clamps an unevaluable or negative answer
-- to zero (CR 107.1b).
data TopOfLibrary = MkTopOfLibrary
  { player :: PlayerRef.PlayerRef,
    count :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
