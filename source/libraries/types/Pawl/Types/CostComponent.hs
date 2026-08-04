module Pawl.Types.CostComponent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | One component of a Pawl.Types.Cost's non-mana part, alongside its mana part
-- (CR 601.2f).
--
-- No component exiles a card from a zone (#108).
--
-- Open-half card data. Pawl.Engine.Cost is the ONLY module that may case on it:
-- the rules core reads the classification (can this be paid? does it require the
-- tap symbol?) and never the identity of a component.
--
-- PARAMETRIC in the keyword for the Filter it carries, and for that type's
-- reason alone -- see Pawl.Types.Filter.
data CostComponent keyword
  = -- | CR 107.5: tap this permanent; one already tapped can't pay the cost.
    -- CR 302.6 gates it on summoning sickness.
    TapThis
  | -- | CR 107.6: {Q}, untap this permanent; one already untapped can't pay the
    -- cost. The mirror of TapThis, and CR 302.6 names the two together, so this
    -- joins TapThis in Pawl.Engine.Cost.requiresSicknessCheck rather than only
    -- in the payment.
    UntapThis
  | -- | CR 701.21a: sacrifice the object the cost is on -- its controller moves it
    -- from the battlefield directly to its owner's graveyard (Mindslaver).
    --
    -- Deliberately NOT `Sacrifice 1 <this permanent>`: CR 602.1a's
    -- self-referential cost names one object and offers no choice, so folding it
    -- into the criterion form would invent a prompt the rules do not have.
    SacrificeThis
  | -- | CR 119.4: pay this much life (Greed), payable only out of a life total
    -- at least that large.
    --
    -- A Natural and not a Quantity: a Quantity's evaluation needs a binding
    -- environment, which a cost has no access to at CR 601.2f time, and no card
    -- in the pool pays a variable amount of life (#99).
    PayLife Natural.Natural
  | -- | CR 701.21a: sacrifice this many permanents matching the Filter (Village
    -- Rites' one creature, Fireblast's two Mountains). The player chooses which,
    -- so this is a prompt and never an engine pick.
    --
    -- The Filter is matched against the PROJECTION, never a printed
    -- characteristic: Blood Moon makes a nonbasic land a Mountain, and it may be
    -- sacrificed as one.
    Sacrifice Natural.Natural (Filter.Filter keyword)
  | -- | CR 601.2f: discard this many cards from hand (Cathartic Reunion's two).
    -- CR 701.9b gives the choice to the discarding player, so this is a prompt
    -- and never an engine pick -- the same shape Sacrifice above has.
    --
    -- No Filter, unlike Sacrifice: "discard a card" names no quality, and the one
    -- card in the pool that prints this cost narrows nothing. A filtered discard
    -- cost would add the field when a card needs it.
    --
    -- A Natural and not a Quantity, for PayLife's reason.
    DiscardCards Natural.Natural
  | -- | CR 702.29a's "Discard this card": discard the card the cost is on, from
    -- the hand it is in.
    --
    -- Deliberately NOT `DiscardCards 1`, and the distinction is the one
    -- SacrificeThis draws against Sacrifice: CR 701.9b gives the discarding
    -- player the choice of WHICH card, so DiscardCards prompts -- while this
    -- names one card and offers nothing to choose. Paying with DiscardCards 1
    -- would invent a prompt the rules do not have, and would let the player
    -- discard some other card to cycle this one.
    --
    -- One of the two components that read a HAND rather than the battlefield
    -- (DiscardCards is the other), so its payability asks about a zone rather
    -- than about control -- CR 108.4 gives a card in a hand no controller at all.
    --
    -- #108 records that a hand-reading component must exclude the spell being
    -- cast from its candidates (CR 601.2a has already moved it to the stack).
    -- That does not arise here: this names the object the cost is on, and a cost
    -- on an object that is no longer in a hand is simply unpayable.
    DiscardThis
  | -- | CR 107.14 / 118: pay N energy counters. Energy-specific, not a general
    -- PayPlayerCounters -- energy is the only player counter ever spent as a
    -- cost. A Natural, not a Quantity, for PayLife's reason; a variable-amount
    -- energy cost is not representable (#121).
    PayEnergy Natural.Natural
  | -- | CR 606.4: put this many loyalty counters on the permanent the cost is on
    -- -- Jace Beleren's `+2`. It carries no recipient, and takes the "This"
    -- suffix SacrificeThis and DiscardThis take, because a loyalty symbol names
    -- that permanent and offers nothing to choose.
    --
    -- TWO arms rather than one signed number. CR 606.4 is a disjunction, CR 606.6
    -- gates only the removing half, and the split keeps loyalty out of the
    -- numeric tower entirely -- a signed count would need a conversion at every
    -- use, and .hlint.yaml bans the unchecked ones. What the split gives up is
    -- CR 606.5's combination of several loyalty costs into one (#496); no
    -- printing has two.
    --
    -- A Natural and not a Quantity, for PayLife's reason.
    AddLoyaltyToThis Natural.Natural
  | -- | CR 606.4's other half: Jace Beleren's `-1` and `-10`. The arm CR 606.6
    -- gates on the permanent already having at least that many loyalty counters,
    -- which Pawl.Engine.Cost.canPayComponent answers by counting
    -- CounterKind.Loyalty on the source.
    RemoveLoyaltyFromThis Natural.Natural
  deriving (Eq, Ord, Show)
