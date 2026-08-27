module Pawl.Types.TurnFaceDown where

import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.ObjectRef as ObjectRef

-- | The payload of Pawl.Types.Effect's TurnFaceDown arm.
--
-- Two fields because CR 708.2 has two halves: WHICH permanents are turned face
-- down, and WHAT the effect lists for them. Backslide lists nothing and takes
-- FaceDownCharacteristics.defaultValue, which is CR 708.2a's 2\/2; Cyber
-- Conversion lists "a 2\/2 Cyberman artifact creature" and carries it.
--
-- WHICH is an ObjectRef and not a bare slot, the shape Pawl.Types.Destroy takes
-- and for the same reason: the printed instruction is not always singular.
-- Weaver of Lies' "any number of target creatures with morph abilities" fills one
-- slot with several recipients, and Ixidron's "all other nontoken creatures" names
-- no slot at all. ObjectRef.InSlot covers the first and ObjectRef.EachMatching the
-- second, and the singular "target creature" of Backslide is InSlot with a slot CR
-- 601.2c filled once. Only the InSlot form has a card in data\/cards; the sweep
-- form is waiting on Ixidron, see #2337.
data TurnFaceDown = MkTurnFaceDown
  { ref :: ObjectRef.ObjectRef,
    characteristics :: FaceDownCharacteristics.FaceDownCharacteristics
  }
  deriving (Eq, Ord, Show)
