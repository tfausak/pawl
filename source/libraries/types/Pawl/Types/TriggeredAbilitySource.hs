module Pawl.Types.TriggeredAbilitySource where

import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility

-- | CR 603.3: a triggered ability on the stack -- the source permanent's id
-- plus the ability. Travels with the object so it resolves even if the source
-- leaves (CR 113.7a).
--
-- A record rather than two positional fields, so the arm has the one payload a
-- codec needs. Pawl.Types.Source is what this is an arm of.
--
-- Not Pawl.Types.TriggerSource, which is the PRE-stack notion: what an ability
-- that has triggered hangs on while CR 603.3b's ordering is still being chosen.
-- This is the on-the-stack counterpart, and unlike that one it carries the
-- ability too.
data TriggeredAbilitySource = MkTriggeredAbilitySource
  { source :: ObjectId.ObjectId,
    ability :: TriggeredAbility.TriggeredAbility Card.Card
  }
  deriving (Eq, Ord, Show)
