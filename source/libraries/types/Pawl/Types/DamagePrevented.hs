module Pawl.Types.DamagePrevented where

import qualified Data.Map as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CandidateId as CandidateId
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Recipient as Recipient

-- | CR 615.1: how much damage a prevention shield stopped, what would have
-- dealt each part of it, who each part was headed for, and WHICH prevention
-- effect stopped it.
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
-- `amounts` is CR 615.13's one application, PER SOURCE and then PER RECIPIENT,
-- copied off Pawl.Types.Prevention: the rule counts one prevention however many
-- simultaneous events and sources it was applied to, so this event is recorded
-- once and carries the whole map. The source is CR 120.1's source of the damage
-- that did not happen, and the only key here a Filter reads -- Samite
-- Ministration's "damage from a black or red source", Judgment of Alexander's
-- "damage from a creature" -- through Pawl.Engine.Projection.viewWithLastKnown,
-- CR 608.2h being live for a source that has since left. A trigger scoped to a
-- recipient reads its own entries (Selfless Squire's "damage that would be dealt
-- to you"), and one scoped to the instance sums the sources its Filter admits
-- (Phyrexian Vindicator's "that much").
data DamagePrevented = MkDamagePrevented
  { by :: CandidateId.CandidateId,
    amounts :: Map.Map ObjectId.ObjectId (Map.Map Recipient.Recipient Natural.Natural)
  }
  deriving (Eq, Ord, Show)
