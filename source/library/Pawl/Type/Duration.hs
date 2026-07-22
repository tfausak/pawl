module Pawl.Type.Duration where

import Pawl.Type.StateCondition (StateCondition)

-- How long a stored continuous effect lasts, as the CARD says it (CR 611.2).
-- PRINTED data: this is what appears in card JSON. The game stores
-- Pawl.Type.Expiry instead, and Pawl.Expiry.arm is the one-way door between
-- them. Static-ability effects carry no Duration -- they last while their
-- source and ability do, which is "while re-derived from the battlefield".
data Duration
  = UntilEndOfTurn -- CR 514.2
  | Indefinite -- CR 611.2a: "lasts until the end of the game" (Magical Hack)
  | -- CR 611.2a: "until your next turn" (Hag of Inner Weakness). "Your" is
    -- resolved to a concrete player by Pawl.Expiry.arm (CR 109.5) -- it cannot
    -- be a PlayerId here, because a printed card does not know one.
    UntilYourNextTurn
  | -- CR 611.2b: "for as long as ...". The duration has a BEGINNING as well as
    -- an end -- "if the 'for as long as' duration never starts, the effect does
    -- nothing" -- which is why Pawl.Expiry.arm returns a Maybe.
    ForAsLongAs StateCondition
  deriving (Eq, Ord, Show)
