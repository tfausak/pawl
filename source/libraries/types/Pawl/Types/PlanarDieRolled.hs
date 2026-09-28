module Pawl.Types.PlanarDieRolled where

import qualified Pawl.Types.PlanarDieFace as PlanarDieFace
import qualified Pawl.Types.PlayerId as PlayerId

-- | The payload of Pawl.Types.GameEvent's PlanarDieRolled arm: CR 901.9's roll,
-- who rolled and what the die showed.
data PlanarDieRolled = MkPlanarDieRolled
  { roller :: PlayerId.PlayerId,
    face :: PlanarDieFace.PlanarDieFace
  }
  deriving (Eq, Ord, Show)
