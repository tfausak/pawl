module Pawl.Types.FaceDownState where

import qualified Pawl.Types.FaceDownCharacteristics as FaceDownCharacteristics
import qualified Pawl.Types.FaceDownReason as FaceDownReason

-- | CR 708.2: the whole of what the game knows about a face-down object -- the
-- ability or rules that ALLOWED it to be face down (CR 708.6), and the
-- characteristics that allower LISTED for it. Morph and the rest list nothing
-- and carry 'FaceDownCharacteristics.defaultValue'.
--
-- The reason is first because CR 708.2's own sentence puts it first: the listed
-- characteristics are "those listed by the ability or rules that allowed the
-- spell or permanent to be face down", so the list is the allower's and not the
-- other way round.
data FaceDownState = MkFaceDownState
  { reason :: FaceDownReason.FaceDownReason,
    listed :: FaceDownCharacteristics.FaceDownCharacteristics
  }
  deriving (Eq, Ord, Show)
