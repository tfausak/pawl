module Pawl.Engine.Ante where

import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameSettings as GameSettings

-- | CR 407.3: when not playing for ante, an ante card "can't be brought into
-- the game from outside the game". Read off the printed face's flag, never its
-- text.
barred :: GameSettings.GameSettings -> Face.Face card -> Bool
barred settings face = Face.anteOnly face && not (GameSettings.ante settings)
