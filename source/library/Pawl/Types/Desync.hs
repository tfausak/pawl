module Pawl.Types.Desync where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Response as Response

-- | Where a recorded transcript stopped answering the prompts the engine actually
-- asked. Pawl.Engine.Replay.replay returns the FIRST one, if any.
--
-- Reporting it at all is the point. Pawl.Engine.Replay.defaultAnswer is
-- deliberately total, which keeps replay free of a partial escape; the price is
-- that a drifted transcript plays out a DIFFERENT game in silence, and a dropped
-- Prompt.Concede changes who WINS (CR 104.2a).
--
-- Structural, and so replay-only: the Prompt GADT's return type stops a LIVE
-- answerer handing back the wrong shape, and only a transcript's untyped Response
-- can mismatch. An answer that is well-typed but ILLEGAL is a different failure,
-- handled where it is asked.
--
-- Only the first, because from that point on every answer comes from
-- defaultAnswer: the run has stopped being a replay, so later reports are
-- consequences rather than independent evidence.
--
-- The Natural is the 0-based position of the prompt in the run's prompt
-- sequence, which is also the number of prompts the transcript did answer.
data Desync
  = -- | The transcript ran out before the engine ran out of questions.
    Exhausted Natural.Natural
  | -- | The next logged response does not answer the prompt being asked -- a
    -- stale or foreign transcript. Carries the offending entry.
    Mismatched Natural.Natural Response.Response
  deriving (Eq, Ord, Show)
