module Pawl.Types.TurnWindow where

import qualified Numeric.Natural as Natural

-- | CR 603.7a: WHICH TURNS a stored delayed triggered ability may fire on, as the
-- game remembers it. The runtime counterpart of the printed Pawl.Types.Onset,
-- the same way Expiry is Duration's and ActiveReplacement is
-- ReplacementEffect's: a card can say "on your next turn", but it cannot name the
-- turn that turns out to be -- and neither can the moment the ability is armed.
--
-- Never appears in card JSON: a card writes an Onset, and only
-- Pawl.Engine.Event.armOnset makes one of these. It does have a runtime codec,
-- whose one caller is a DelayedTrigger's, since a stored entry has to survive the
-- trip.
--
-- The three arms are one LIFE CYCLE rather than three independent choices, and
-- it only ever narrows: AnyTurn stands still, and an onset-gated entry goes
-- ControllersNextTurn -> OnTurn n -> gone. Pawl.Engine.Event is the only engine
-- module that may case on this type, the standing Pawl.Engine.Expiry has over
-- Expiry; Pawl.Engine.Engine.beginTurnOf asks it for the transitions at the one
-- moment they can happen.
data TurnWindow
  = -- | Onset.Immediately: no turn restriction at all, which is CR 603.7a's floor
    -- and every delayed ability in the pool but Meandering Towershell's. The
    -- floor itself is the event scan's watermark, not this type's business.
    AnyTurn
  | -- | Onset.FromYourNextTurn, before that turn has arrived: the entry watches
    -- for nothing yet, whatever its condition matches.
    --
    -- No player of its own, because DelayedTrigger.controller is already the
    -- answer (CR 603.7d-f bake it in at arming) and two copies could disagree.
    -- This is the exact mirror of Expiry.AtTurnOf, which ENDS a stored effect at
    -- the boundary this one OPENS a delayed ability at.
    --
    -- No turn number of its own either, and that is the point: the number of the
    -- controller's next turn is not knowable when the ability is armed. An
    -- intervening opponent's turn and an extra turn (CR 500.7) each move it, and
    -- a seat whose turn never begins at all (CR 800.4k) does not supply it -- so
    -- the only honest thing to store is the boundary, and the number when the
    -- turn arrives.
    ControllersNextTurn
  | -- | The same onset once that turn has begun: the number
    -- Pawl.Engine.Event.settleOnsets read off GameState.turnNumber as it did, and
    -- NO other turn.
    --
    -- Closed at both ends, which a lower bound cannot be. The printed phrase
    -- names ONE turn, so if that turn's trigger event never happens -- Stonehorn
    -- Dignitary taking the combat phase away -- CR 614.10a's first sentence
    -- settles it, and CR 603.7a says the same from the ability's side. Firing it
    -- on the controller's NEXT turn instead would be firing it on a turn the card
    -- does not name.
    --
    -- CR 614.10a's SECOND sentence, on waiting for the first occurrence that is
    -- not skipped, is why the two halves are settled at different moments. The
    -- only "next" Meandering Towershell prints is the TURN, so it is the turn
    -- that waits, which is what settleOnsets running per turn BEGUN delivers. The
    -- step inside that turn is not a "next" occurrence of anything, so sentence
    -- one governs it.
    --
    -- An entry whose turn has passed is dropped rather than left to fail the
    -- comparison forever: it can no longer fire, and CR 603.7b's one shot is not
    -- the only way an entry can end.
    OnTurn Natural.Natural
  deriving (Eq, Ord, Show)
