module Pawl.Types.SchemeSetInMotion where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | The payload of Pawl.Types.GameEvent's SchemeSetInMotion arm: CR 701.32's
-- archenemy and the scheme card they set in motion.
data SchemeSetInMotion = MkSchemeSetInMotion
  { player :: PlayerId.PlayerId,
    scheme :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
