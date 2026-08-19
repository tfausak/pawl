module Pawl.Types.Duration where

import qualified Pawl.Types.Condition as Condition

-- | How long a stored continuous effect lasts, as the CARD says it (CR 611.2).
-- PRINTED data: this is what appears in card JSON. The game stores
-- Pawl.Types.Expiry instead, and Pawl.Engine.Expiry.arm is the one-way door between
-- them. A static ability's effect normally carries no Duration -- it lasts
-- while its source and ability do, which is "while re-derived from the
-- battlefield" -- and the one exception is StaticAbility.lingers, the clause
-- that gives such an effect a duration to keep running out AFTER its permanent
-- has gone.
data Duration
  = UntilEndOfTurn -- CR 514.2
  | Indefinite -- CR 611.2a: "lasts until the end of the game" (Magical Hack)
  | -- | CR 611.2a: "until your next turn" (Hag of Inner Weakness). "Your" is
    -- resolved to a concrete player by Pawl.Engine.Expiry.arm (CR 109.5) -- it cannot
    -- be a PlayerId here, because a printed card does not know one.
    UntilYourNextTurn
  | -- | CR 611.2a: "until the end of your next turn" (Soulfire Eruption), which
    -- ends a whole turn later than the arm above: as that turn ENDS, in its
    -- cleanup step (CR 514), rather than as it begins. Both "your"s are resolved
    -- the same way, by Pawl.Engine.Expiry.arm.
    UntilEndOfYourNextTurn
  | -- | CR 611.2b: "for as long as ...". The duration has a BEGINNING as well as
    -- an end -- "if the 'for as long as' duration never starts, the effect does
    -- nothing" -- which is why Pawl.Engine.Expiry.arm returns a Maybe.
    ForAsLongAs Condition.Condition
  | -- | CR 500.5a / 511.2: expires at the end of the combat PHASE, not at the
    -- beginning of the end of combat step. Jade Statue's animation.
    --
    -- Nullary, and the only CR 500.5 WINDOW a card can print: the stored
    -- Pawl.Types.Expiry it arms to can name any window that rule can end, but no
    -- card in the pool prints the others (#353).
    --
    -- WHICH combat phase is not carried. CR 500.8 permits more than one in a
    -- turn, and the sweep ends this effect at the first whose end it sees; every
    -- producer can only be activated during combat, so that is the phase the
    -- effect was created in. One armed OUTSIDE combat is unspecified here (#525).
    UntilEndOfCombat
  deriving (Eq, Ord, Show)
