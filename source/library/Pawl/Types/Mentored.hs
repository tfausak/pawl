module Pawl.Types.Mentored where

import qualified Pawl.Types.ObjectId as ObjectId

-- | CR 702.134a's mentor: which creature mentored, and which it mentored.

-- BOTH fields are an ObjectId and they are NOT interchangeable, so they are named
-- rather than positional: a swap would credit the wrong creature.
data Mentored = MkMentored
  { mentor :: ObjectId.ObjectId,
    mentored :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
