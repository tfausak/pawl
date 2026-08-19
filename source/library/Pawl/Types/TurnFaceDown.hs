module Pawl.Types.TurnFaceDown where

import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's TurnFaceDown arm.
--
-- Two fields because CR 708.2 has two halves: WHICH permanent is turned face
-- down, and WHAT the effect lists for it. Backslide lists nothing and takes
-- FaceDownCharacteristics.defaultValue, which is CR 708.2a's 2\/2; Cyber
-- Conversion lists "a 2\/2 Cyberman artifact creature" and carries it.
data TurnFaceDown = MkTurnFaceDown
  { slot :: SlotName.SlotName,
    characteristics :: FaceDownCharacteristics.FaceDownCharacteristics
  }
  deriving (Eq, Ord, Show)
