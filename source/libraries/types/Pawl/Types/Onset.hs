module Pawl.Types.Onset where

-- | CR 603.7: when a delayed triggered ability BECOMES ARMED, as the card says it
-- -- the half of a printed clause like "at the beginning of the declare
-- attackers step on your next turn" that a TriggerCondition cannot carry, since
-- that condition says WHICH event and this says which occurrence of it counts.
--
-- Duration's mirror image, and printed data for the same reason: a card can say
-- "on your next turn", but it cannot name the turn number that turns out to be,
-- so Pawl.Engine.Resolve resolves this to a concrete turn as the ability is
-- armed (DelayedTrigger.notBefore).
--
-- Immediately is the default and by far the common case: Tidal Wave's "at the
-- beginning of the next end step" and Full Throttle's "at the beginning of each
-- combat this turn" both watch for their event from the moment they are created,
-- which is CR 603.7a's floor ("a delayed triggered ability won't trigger until
-- it has actually been created, even if its trigger event occurred just
-- beforehand") and the only gate those two need.
--
-- FromYourNextTurn is Meandering Towershell's "at the beginning of the declare
-- attackers step on your next turn". The gate is NOT vacuous and not
-- expressible by the trigger condition alone: TriggerCondition.StepBegins
-- carries a TurnScope, and ControllersTurn admits the very turn the ability was
-- armed on -- so an extra combat phase in that same turn (Relentless Assault,
-- Aggravated Assault, Full Throttle, Aurelia) would fire it early. "Next" is a
-- fact about WHEN THE ABILITY WAS CREATED, which only the arming moment knows.
--
-- On the OPCODE (Effect.ArmDelayedTrigger) rather than on the ability's
-- condition, for the reason the Duration beside it is there: the temporal
-- envelope a delayed ability is armed inside is what the ARMING says, while the
-- ability's own condition says which event it watches for. A TurnScope arm
-- meaning "next" would also be representable -- and meaningless -- on an
-- ordinary printed triggered ability, which has no creation moment to be next
-- to.
data Onset
  = Immediately
  | -- | Duration.UntilYourNextTurn read as a beginning instead of an end.
    --
    -- WHAT THIS ARM ENFORCES ON ITS OWN is the NEXT half of "your next turn" and
    -- only that half: Pawl.Engine.Resolve turns it into a turn NUMBER
    -- (DelayedTrigger.notBefore, the successor of the turn it was armed on) and
    -- Pawl.Engine.Event.delayedPending withholds the match until the live turn
    -- number reaches it. A turn number cannot say WHOSE turn it is, so nothing
    -- here rules out an intervening opponent's turn.
    --
    -- THE "YOUR" HALF comes from a different field entirely: the delayed
    -- ability's own TriggerCondition.StepBegins carrying
    -- TurnScope.ControllersTurn (CR 109.5's "you", which CR 603.3a and CR
    -- 603.7d-f fix to the ability's controller). The two COLLABORATE and neither
    -- is redundant -- the scope alone admits the arming turn itself, which is
    -- what an extra combat phase would fire early (see the paragraph above), and
    -- this alone admits any later turn whatever.
    --
    -- So this arm paired with a condition that is not controller-scoped makes
    -- the constructor's own name false. That pairing is REJECTED rather than
    -- assumed: Pawl.CardSpec's "every delayed ability armed for YOUR next turn
    -- is controller-scoped" is the lint, and Pawl.Engine.Event.controllerTurnScoped
    -- is the classification it asks.
    FromYourNextTurn
  deriving (Eq, Ord, Show)
