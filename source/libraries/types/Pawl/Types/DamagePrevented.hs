module Pawl.Types.DamagePrevented where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.1: how much damage a prevention shield stopped, who it was headed
-- for, what would have dealt it, and WHICH prevention effect stopped it.
--
-- `by` is the applying instance's CR 614.5 identity, copied off
-- Pawl.Types.Prevention rather than derived: it is the same key
-- Pawl.Engine.Replacement.groupPreventions collapsed the batch by, so one entry
-- and one identity are the same fact said twice. It is what CR 615.13's
-- "prevented THIS WAY" compares against -- Phyrexian Vindicator's trigger fires
-- for its own ability's prevention and stays silent for anybody else's -- while
-- Selfless Squire ignores it, its own 2016-11-08 ruling saying "any effect that
-- uses the word 'prevent' will cause it to trigger".
--
-- `source` is CR 120.1's source of the damage that did not happen, copied off
-- Pawl.Types.Prevention beside `by`. It is what Samite Ministration's "damage
-- from a black or red source" asks about, and the only field here a Filter
-- reads -- Pawl.Engine.Event resolves it through
-- Pawl.Engine.Projection.viewWithLastKnown, CR 608.2h being live for a source
-- that has since left.
--
-- Not implemented: one prevention effect covering two DIFFERENT sources in a
-- single CR 615.13 batch reports only one of them here, because
-- Pawl.Engine.Replacement.groupPreventions collapses that batch to one entry
-- (#2287).
data DamagePrevented = MkDamagePrevented
  { by :: CandidateId.CandidateId,
    source :: ObjectId.ObjectId,
    recipient :: Recipient.Recipient,
    amount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
