module Pawl.Type.TriggerCondition where

import Pawl.Type.Phase (Phase)
import Pawl.Type.StateCondition (StateCondition)
import Pawl.Type.TurnScope (TurnScope)

-- CR 603.2: the pattern that fires a triggered ability. Only Pawl.Event may case
-- on it for RULES purposes; Pawl.Codec also cases on every constructor, but only
-- as the JSON data boundary (encode/decode), not to decide game behaviour.
data TriggerCondition
  = -- CR 603.6a: "when this ... enters [the battlefield]" -- fires when the object
    -- BEARING the ability enters. Self-scoped: the scan checks every permanent
    -- (CR 603.6a), so the bearer's identity is part of the match, not an accident
    -- of which object the scan happened to visit. A general "whenever a [type]
    -- enters" is a future condition.
    SelfEnters
  | -- CR 603.2b: "at the beginning of [each|your] <step>". Matched against a
    -- GameEvent.StepBegan; the TurnScope decides whose turn qualifies.
    StepBegins Phase TurnScope
  | -- CR 603.8: a STATE trigger -- it fires whenever its condition is true, not
    -- when an event occurs. "It doesn't trigger again until the ability has
    -- resolved, has been countered, or has otherwise left the stack", which is why
    -- Pawl.Event derives armedness from the stack rather than storing it.
    StateIs StateCondition
  | -- CR 603.2 / 509-510: the bearer dealt combat damage to a player. Rides P4's
    -- event history -- combat damage already records a DamageDealt event.
    SelfDealsCombatDamageToPlayer
  | -- CR 725.2: a creature dealt combat damage to the monarch. NOT bearer-scoped
    -- (any creature); matched only via Pawl.Monarch.inherentMatch, never through a
    -- card's bearer. Rides P4's DamageDealt history.
    CreatureDealtCombatDamageToMonarch
  deriving (Eq, Ord, Show)
