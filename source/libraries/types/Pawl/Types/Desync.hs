module Pawl.Types.Desync where

import Numeric.Natural (Natural)
import Pawl.Types.Response (Response)

-- Where a recorded transcript stopped answering the prompts the engine actually
-- asked. Pawl.Replay.replay returns the FIRST one, if any.
--
-- Reporting it at all is the point. Pawl.Replay.defaultAnswer is deliberately
-- total, which is what keeps replay free of a partial escape; the price is that
-- a drifted transcript plays out a DIFFERENT game in silence. For most prompts
-- that under- or over-fills a choice, but a dropped Prompt.Concede changes who
-- WINS (CR 104.2a), so silence is not an option a caller should have to opt out
-- of.
--
-- Structural, and so replay-only: an interpreter is `Prompt r -> m r`, and the
-- GADT's return type stops a LIVE answerer handing back the wrong shape at all.
-- Only a transcript can mismatch, because it stores an untyped Response that
-- Pawl.Replay.decode has to turn back into an `r`. An answer that is well-typed
-- but ILLEGAL is a different failure, and is handled where it is asked rather
-- than here -- see Pawl.Engine.priorityLoop's "FILTERED, NOT TRUSTED".
--
-- Only the first, because from that point on every answer comes from
-- defaultAnswer: the run has stopped being a replay of the recorded game, and
-- later reports are consequences of this one rather than independent evidence. A
-- Mismatched entry is not consumed, so in practice every later prompt meets it
-- too and the whole remaining transcript is stranded behind it.
--
-- The Natural is the 0-based position of the prompt in the run's prompt
-- sequence, which is also the number of prompts the transcript did answer.
data Desync
  = -- The transcript ran out before the engine ran out of questions.
    Exhausted Natural
  | -- The next logged response does not answer the prompt being asked -- a
    -- stale or foreign transcript. Carries the offending entry.
    Mismatched Natural Response
  deriving (Eq, Show)
