module Pawl.Types.TriggerLimit where

-- | The printed riders "This ability triggers only once each turn" and "This
-- ability triggers only once", as a property of the ABILITY rather than of the
-- trigger event.
--
-- There is NO comprehensive rule for either clause. CR 603.2h is the nearest and
-- is a different sentence -- it qualifies an INSTRUCTION ("Do this only once each
-- turn"), so the ability still triggers and its effect declines to happen. CR
-- 702.179d prints the per-turn rider verbatim on the rulebook's own speed
-- ability, which is as close to a rule as the phrase gets, and this type says so
-- rather than manufacturing a citation. The per-game rider (Acrobatic
-- Cheerleader) sits in the same position with no rule at all.
--
-- Deliberately NOT Pawl.Types.TriggerFrequency, which the three arms below would
-- otherwise duplicate. That type narrows a trigger EVENT -- Aurelia, the
-- Warleader "attacks for the first time each turn" -- and the two are
-- distinguishable: an ability gained after the turn's first matching event has
-- already passed still triggers under this rider and never does under that
-- narrowing.
data TriggerLimit
  = -- | The ability triggers on every occurrence of its trigger event
    -- (CR 603.2c). Every printing but the riders' bearers.
    Unlimited
  | -- | At most one triggering per turn. Spent on TRIGGERING, so an instance
    -- countered on the stack has still spent the turn's one.
    OncePerTurn
  | -- | At most one triggering per game. Spent on TRIGGERING like the arm above,
    -- and over a record that outlives the turn handoff
    -- (Pawl.Types.GameState.triggeredThisGame).
    OncePerGame
  deriving (Bounded, Enum, Eq, Ord, Show)
