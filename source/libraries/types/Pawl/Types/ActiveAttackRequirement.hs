module Pawl.Types.ActiveAttackRequirement where

import qualified Pawl.Types.Expiry as Expiry
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 508.1d / 613.11: a stored, resolution-generated ATTACKING REQUIREMENT,
-- held in GameState.attackRequirements. The attack-axis analogue of
-- Pawl.Types.ActiveBlockRequirement, and the carrier that says WHAT the creature
-- has to attack: Alluring Siren's "target creature an opponent controls attacks
-- you this turn if able".
--
-- Pawl.Types.AttackRequirement, the PRINTED carrier, states only CR 508.1d's
-- subject, and says there why the object axis lives here instead (#2014).
--
-- Both axes are bare, one ObjectId and one PlayerId, for
-- ActiveBlockRequirement's reason: rule 508.1d's producer here names its
-- creature by TARGETING it (CR 115.1), so there is no set for CR 611.2c's
-- carve-out to keep dynamic.
--
-- The object is a PlayerId rather than a Pawl.Types.AttackTarget, and the
-- narrowing is the rule rather than a convenience: Alluring Siren's ruling is
-- that a creature able to attack either you or a planeswalker you control must
-- attack YOU, so "attacks you" is obeyed by exactly one of CR 508.1b's
-- announcements. Goad (CR 701.15b) wants the COMPLEMENT of a seat instead --
-- "a player other than the controller of the permanent that caused it to be
-- goaded" -- which this field cannot say (#1388).
--
-- `expiry` decides when a Pawl.Engine.Expiry sweep drops it (CR 514.2, 611.2a,
-- 611.2b); Alluring Siren's "this turn" arms Expiry.AtCleanup.
--
-- `timestamp` is stored for ActiveBlockRequirement's reason: CR 613.11 orders by
-- timestamp (CR 613.7), and nothing observes it yet because two requirements
-- cannot conflict -- CR 508.1d counts them and never resolves them against each
-- other.
--
-- Runtime-only, like Expiry and ActiveBlockRequirement: no codec, which keeps a
-- stored value out of a card file and a printed value out of the store.
data ActiveAttackRequirement = MkActiveAttackRequirement
  { source :: ObjectId.ObjectId,
    timestamp :: Timestamp.Timestamp,
    expiry :: Expiry.Expiry,
    -- | The creature that must attack -- CR 508.1d's subject axis, which the
    -- printed carrier states as an Affected for the reason given above.
    attacker :: ObjectId.ObjectId,
    -- | The player it must attack -- CR 508.1d's object axis, read through CR
    -- 508.1b's announcement.
    defender :: PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)
