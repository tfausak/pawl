module Pawl.Types.ReplacementCandidate where

import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect

-- | One replacement effect instance as the CR 616.1 loop sees it: what it does,
-- whose it is (CR 109.5's "you", which every ControllerRelation pattern reads),
-- and which instance it is (CR 614.5). `source` is derivable from `identity` but
-- is kept explicit -- every applicability test and the ChooseReplacement payload
-- read it directly.
data ReplacementCandidate = MkReplacementCandidate
  { identity :: CandidateId.CandidateId,
    effect :: ReplacementEffect.ReplacementEffect,
    source :: ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
