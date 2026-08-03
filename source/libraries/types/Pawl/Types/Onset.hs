module Pawl.Types.Onset where

-- | CR 603.7: when a delayed triggered ability BECOMES ARMED, as the card says it
-- -- the half of a printed clause like "at the beginning of the declare
-- attackers step on your next turn" that a TriggerCondition cannot carry, since
-- that condition says WHICH event and this says which occurrence of it counts.
--
-- Duration's mirror image, and printed data for the same reason: a card can say
-- "on your next turn", but it cannot name the turn that turns out to be, so
-- Pawl.Engine.Event.armOnset turns this into the stored Pawl.Types.TurnWindow
-- (DelayedTrigger.window) that the game fills in as the turns arrive.
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
-- fact about WHEN THE ABILITY WAS CREATED, and nothing the ability watches for
-- says it: the arming is what records that this turn is not the one.
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
    -- BOTH HALVES of "your next turn" are stored, and neither is stored as a
    -- number read at arming: Pawl.Engine.Event.armOnset turns this into
    -- TurnWindow.ControllersNextTurn, which names the boundary rather than the
    -- turn, and Pawl.Engine.Event.settleOnsets replaces it with the one turn
    -- number that boundary turns out to be as that turn begins. "Your" is the
    -- entry's own DelayedTrigger.controller (CR 109.5's "you", which CR 603.3a and
    -- CR 603.7d-f fix to the ability's controller); "next" is the arrival itself.
    --
    -- The delayed ability's own TriggerCondition.StepBegins carries
    -- TurnScope.ControllersTurn as well, which the settled window makes
    -- redundant for FIRING -- the window already admits no other player's turn.
    -- The pairing is still lint-enforced rather than left to the window
    -- (Pawl.CardSpec's "every delayed ability armed for YOUR next turn is
    -- controller-scoped", over Pawl.Engine.Event.controllerTurnScoped), because a
    -- card that armed this onset over an EachTurn condition would have its
    -- printed "each" silently narrowed to the controller's turn. Card data must
    -- say what the card says instead of leaning on the engine to mean it.
    FromYourNextTurn
  deriving (Eq, Ord, Show)
