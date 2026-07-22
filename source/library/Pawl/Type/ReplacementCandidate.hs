module Pawl.Type.ReplacementCandidate where

import Pawl.Type.CandidateId (CandidateId)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.ReplacementEffect (ReplacementEffect)

-- One replacement effect instance as the CR 616.1 loop sees it: what it does,
-- whose it is (CR 109.5's "you", which every ControllerRelation pattern reads),
-- and which instance it is (CR 614.5). `source` is derivable from `identity` but
-- is kept explicit -- every applicability test and the ChooseReplacement payload
-- read it directly.
data ReplacementCandidate = MkReplacementCandidate
  { identity :: CandidateId,
    effect :: ReplacementEffect,
    source :: ObjectId
  }
  deriving (Eq, Ord, Show)
