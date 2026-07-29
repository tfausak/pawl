module Pawl.Type.Desync where

import Numeric.Natural (Natural)
import Pawl.Type.Response (Response)

-- Where a recorded transcript stopped answering the prompts the engine actually
-- asked. Pawl.Replay.replay returns the FIRST one, if any.
--
-- Only the first, because from that point on every answer comes from
-- Pawl.Replay.defaultAnswer: the run has stopped being a replay of the recorded
-- game, and later reports are consequences of this one rather than independent
-- evidence. A Mismatched entry is not consumed, so in practice every later
-- prompt meets it too and the whole remaining transcript is stranded behind it.
--
-- Reporting it at all is the point (#144). defaultAnswer is deliberately total,
-- which is what keeps replay free of a partial escape; the price is that a
-- drifted transcript plays out a DIFFERENT game in silence. For most prompts
-- that under- or over-fills a choice, but a dropped Prompt.Concede changes who
-- WINS (CR 104.2a), so silence is not an option a caller should have to opt out
-- of.
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
