module Pawl.Types.HandAction where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Effect as Effect

-- | CR 103.5b / CR 103.6: one action a card grants from its owner's HAND before
-- the game begins -- what taking it does, and the clause that says when it may
-- be taken at all.
--
-- The carrier of Pawl.Types.Face's mulliganActions and openingHandActions, which
-- held a bare list of effects until Gemstone Caverns printed a gate, see #186.
-- ONE type for both fields, because Pawl.Engine.Mulligan.actionsFor is a single hand-scanner
-- taking the field as a selector, so the two must agree in shape.
--
-- Parametric in @card@ for Pawl.Types.Effect's reason: Card embeds the payload,
-- so a concrete @Effect Card@ here would make the two modules mutually importing.
data HandAction card = MkHandAction
  { -- | Gemstone Caverns' "you're not the starting player" -- the half of the
    -- printed sentence that is not "if this card is in your opening hand", which
    -- the window itself already answers by scanning the hand.
    --
    -- Read BEFORE THE OFFER and not at the performance, which is where CR 103.6
    -- puts it: a card whose clause is false allows the player no action, so there
    -- is nothing to be asked about. Pawl.Engine.Mulligan.handWindowExcept filters
    -- on it, beside CR 103.6b's cap, and re-reads it on every pass of the loop --
    -- so an earlier action of the same window that changed the board is seen.
    --
    -- Evaluated against the CARD IN HAND with the acting player as CR 109.5's
    -- "you" -- that rule's OWNER clause, a card in a hand having no controller.
    -- Nothing is the unmarked case every other action in the corpus takes.
    condition :: Maybe Condition.Condition,
    -- | What taking the action does, in written order. A list rather than a Seq
    -- for the reason the field it replaced was one: this is read straight off the
    -- face and folded once.
    effects :: [Effect.Effect card]
  }
  deriving (Eq, Ord, Show)
