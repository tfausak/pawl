module Pawl.Types.ReplacementCandidate where

import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ReplacementEffect as ReplacementEffect
import qualified Pawl.Types.ReplacementOrigin as ReplacementOrigin

-- | One replacement effect instance as the CR 616.1 loop sees it: what it does,
-- whose it is (CR 109.5's "you", which every ControllerRelation pattern reads),
-- which instance it is (CR 614.5), and whether it is one of CR 614.15's
-- self-replacement effects (CR 616.1a's step). `source` is derivable from
-- `identity` but is kept explicit -- every applicability test and the
-- ChooseReplacement payload read it directly.
--
-- `origin` rides here rather than on ReplacementEffect because CR 614.15 is about
-- which ability CREATED an effect, not about what the effect does; see
-- Pawl.Types.ReplacementOrigin. It is what lets bucketOf answer CR 616.1a without
-- the rules core ever asking what an effect IS.
data ReplacementCandidate = MkReplacementCandidate
  { identity :: CandidateId.CandidateId,
    effect :: ReplacementEffect.ReplacementEffect,
    source :: ObjectId.ObjectId,
    -- | CR 109.5's "you" for this instance, from whichever of the two segments
    -- can answer it: a permanent's static ability derives it from `source`'s
    -- current controller, and a floating row carries it baked (see
    -- Pawl.Types.ActiveReplacement). Nothing only where a permanent-sourced
    -- instance's source has left the board, which every pattern reading it then
    -- treats as matching nothing rather than everything.
    --
    -- NOT derivable from `source` at the point of use, which is the whole reason
    -- it is a field: Gather Specimens' row outlives the spell that installed it.
    controller :: Maybe PlayerId.PlayerId,
    origin :: ReplacementOrigin.ReplacementOrigin
  }
  deriving (Eq, Ord, Show)
