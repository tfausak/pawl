module Pawl.Types.Duration where

import Pawl.Types.Condition (Condition)

-- How long a stored continuous effect lasts, as the CARD says it (CR 611.2).
-- PRINTED data: this is what appears in card JSON. The game stores
-- Pawl.Types.Expiry instead, and Pawl.Engine.Expiry.arm is the one-way door between
-- them. Static-ability effects carry no Duration -- they last while their
-- source and ability do, which is "while re-derived from the battlefield".
data Duration
  = UntilEndOfTurn -- CR 514.2
  | Indefinite -- CR 611.2a: "lasts until the end of the game" (Magical Hack)
  | -- CR 611.2a: "until your next turn" (Hag of Inner Weakness). "Your" is
    -- resolved to a concrete player by Pawl.Engine.Expiry.arm (CR 109.5) -- it cannot
    -- be a PlayerId here, because a printed card does not know one.
    UntilYourNextTurn
  | -- CR 611.2b: "for as long as ...". The duration has a BEGINNING as well as
    -- an end -- "if the 'for as long as' duration never starts, the effect does
    -- nothing" -- which is why Pawl.Engine.Expiry.arm returns a Maybe.
    ForAsLongAs Condition
  | -- CR 500.5a: "Effects that last 'until end of combat' expire at the end of
    -- the combat PHASE, not at the beginning of the end of combat step." CR
    -- 511.2 says the same thing from the step's side. Jade Statue's animation.
    --
    -- Nullary, and the only end-of-window duration a card can print. The stored
    -- Pawl.Types.Expiry it arms to carries a Pawl.Types.PhaseSelector and so can
    -- name any window CR 500.5 can end -- a step as well as a phase -- but no
    -- printed arm reaches the others, because no card in the pool prints them
    -- (#353's own list: "until end of this step" has no producer).
    --
    -- WHICH combat phase is not carried, because it never has to be: CR 500.8
    -- permits more than one combat phase in a turn, and the sweep ends this
    -- effect at the first combat phase whose end it sees. Every producer is an
    -- ability that can only be activated DURING combat (Jade Statue prints
    -- "Activate only during combat"), so that phase is always the one the
    -- effect was created in. An "until end of combat" armed OUTSIDE a combat
    -- phase is unreachable from the pool and unspecified here (#525).
    UntilEndOfCombat
  deriving (Eq, Ord, Show)
