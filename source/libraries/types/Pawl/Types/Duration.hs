module Pawl.Types.Duration where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Keyword as Keyword

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
  | -- | CR 611.2a: "You may pay {U} to end this effect", the clause every Licid
    -- prints. The duration a spell or ability states need not be a window of the
    -- turn at all -- this one ends when a player takes CR 116.2c's special
    -- action, which Pawl.Types.Action's EndEffect carries and
    -- Pawl.Engine.EndEffect performs.
    --
    -- The COST rides here because CR 116.2c fixes neither price nor timing: "for
    -- as long as the effect allows it", and the effect is this. All twelve Licids
    -- say one mana symbol, but the field is a whole Pawl.Types.Cost --
    -- SpecialAction.IgnoreThisUntilEndOfTurn's shape one rule over, so the two
    -- pay-to-take actions are paid through one Pawl.Engine.Cost call.
    --
    -- No sweep of Pawl.Engine.Expiry ends it: the stored counterpart
    -- (Expiry.WhenPaid) is what the special action looks for, and nothing about
    -- the turn's structure ends it. Indefinite is therefore the WRONG arm for a
    -- Licid even though both outlast every window -- an Expiry.Never cannot be
    -- keyed on, so the performer could not find the effects the payment ends.
    UntilPaid (Cost.Cost Keyword.Keyword)
  deriving (Eq, Ord, Show)
