module Pawl.Types.DamagePrevented where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.1: how much damage a prevention shield stopped, who it was headed
-- for, and WHICH prevention effect stopped it.
--
-- `by` is the applying instance's CR 614.5 identity, copied off
-- Pawl.Types.Prevention rather than derived: it is the same key
-- Pawl.Engine.Replacement.groupPreventions collapsed the batch by, so one entry
-- and one identity are the same fact said twice. It is what CR 615.13's
-- "prevented THIS WAY" compares against -- Phyrexian Vindicator's trigger fires
-- for its own ability's prevention and stays silent for anybody else's -- while
-- Selfless Squire ignores it, its own 2016-11-08 ruling saying "any effect that
-- uses the word 'prevent' will cause it to trigger".
data DamagePrevented = MkDamagePrevented
  { by :: CandidateId.CandidateId,
    recipient :: Recipient.Recipient,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
