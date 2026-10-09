module Pawl.Types.TriggeredAbilitySource where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Timestamp as Timestamp
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
    ability :: TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card),
    -- | CR 603.7a: when the DELAYED ability this object came from was created,
    -- and Nothing for an ability the source itself has. That is a
    -- CLASSIFICATION of how the object got here rather than which ability it
    -- is, and it is the only thing on the stack that tells the two kinds apart:
    -- CR 701.27f and CR 701.28e measure a delayed ability's transform or
    -- convert from its creation and every other ability's from CR 613.7d's
    -- placement stamp (Pawl.Engine.Resolve.Effect.alreadyTurnedFor).
    --
    -- Carried on the object rather than looked up, the entry it came from being
    -- gone by resolution: an entry with no stated duration is retired as it
    -- fires (Pawl.Engine.Event.Trigger.delayedPending).
    createdAt :: Maybe Timestamp.Timestamp,
    -- | CR 113.7a / 603.7: the delayed abilities the source's rules text
    -- declared as this object was put on the stack (PC.delayedAbilities),
    -- frozen with it: the ability exists independently of its source, so a copy
    -- effect or text change on the source afterwards does not change what its
    -- arms create.
    delayed :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card))
  }
  deriving (Eq, Ord, Show)
