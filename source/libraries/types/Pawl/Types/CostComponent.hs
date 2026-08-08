module Pawl.Types.CostComponent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Filter as Filter

-- | One component of a Pawl.Types.Cost's non-mana part, alongside its mana part
-- (CR 601.2f).
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
  | -- | CR 702.122a's cost half: tap ANY NUMBER of untapped permanents matching
    -- the Filter, chosen so that their TOTAL power is the Natural or greater.
    -- Crew 6's "tap any number of other untapped creatures you control with
    -- total power 6 or greater" is the printing, and the whole of "other
    -- untapped creatures you control" rides the Filter rather than this
    -- constructor -- CR 702.122a's "other" is `Not IsSource`, its "untapped" is
    -- `Not IsTapped` and its "you control" is `ControlledBy You`, all three
    -- already in the vocabulary.
    --
    -- NOT `Sacrifice`-shaped, and the difference is the whole point of a second
    -- arm: Sacrifice's Natural is HOW MANY objects, which the payer must match
    -- exactly, while this one is a THRESHOLD on an aggregate of the chosen set.
    -- CR 702.122a's "or greater" makes overpaying legal, so the number of
    -- objects is not determined by the cost at all -- one 7-power creature and
    -- three 2-power ones are both legal answers to crew 6.
    --
    -- The aggregate lives here and not in the Filter, because a Filter is a
    -- predicate over ONE candidate (see Pawl.Types.Filter) and total power is a
    -- property of the chosen SET. Pawl.Types.Aggregation, the other aggregate
    -- vocabulary, has no summing arm either.
    --
    -- POWER specifically, and not a general characteristic: CR 702.122a names
    -- power, and it is the only aggregate threshold any cost in the pool states.
    -- Convoke (CR 702.51) is the other any-number tap cost and is a different
    -- shape entirely -- each tapped creature pays one symbol, with no threshold
    -- to reach -- so it will not reuse this arm.
    --
    -- A Natural and not a Quantity, for PayLife's reason above.
    TapForTotalPower Natural.Natural (Filter.Filter keyword)
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
    -- A hand-reading component must exclude the spell being cast from its
    -- candidates, since CR 601.2a has already moved it to the stack -- see
    -- Pawl.Engine.Cost.discardCandidates. That does not arise here: this names
    -- the object the cost is on, and a cost on an object that is no longer in a
    -- hand is simply unpayable.
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
  | -- | CR 406.2 as a cost: exile the card the cost is on, from the graveyard it
    -- is in. Loxodon Surveyor's "{3}, Exile this card from your graveyard: Draw a
    -- card" is the printing. CR 601.2f's list of what a cost may include ends in
    -- "and so on", so exiling is a cost by CR 118.1's general reading -- "an
    -- action or payment necessary to take another action" -- and not by being
    -- named.
    --
    -- DiscardThis's sibling in every respect -- it names ONE card and offers
    -- nothing to choose, so it is the "This" half of the axis whose general form
    -- is ExileCardsFromGraveyard below, exactly as DiscardThis is to
    -- DiscardCards. Its payability asks about a ZONE rather than about control,
    -- since CR 108.4 leaves a card in a graveyard with no controller to ask
    -- about.
    --
    -- The ZONE is in the constructor rather than a field, and that is what makes
    -- CR 113.6m answerable off the cost alone: "an ability whose cost or effect
    -- specifies that it moves the object it's on out of a particular zone
    -- functions only in that zone" is the rule that puts the Surveyor's ability in
    -- the graveyard, and Pawl.Engine.Cost.zoneFunctionedFrom reads it here. A
    -- second zone would be a second constructor rather than a parameter, so no arm
    -- of that reading is ever unreachable.
    ExileThisFromGraveyard
  | -- | CR 406.2 again, in its CHOOSING form: exile this many cards matching the
    -- Filter from the paying player's graveyard. Headless Skaab's "As an
    -- additional cost to cast this spell, exile a creature card from your
    -- graveyard" is the printing. A cost by CR 118.1's general reading -- "an
    -- action or payment necessary to take another action" -- for
    -- ExileThisFromGraveyard's stated reason, since CR 601.2f's list of what a
    -- cost may include ends in "and so on" rather than naming exile.
    --
    -- Sacrifice's shape (a count plus a criterion) and not
    -- ExileThisFromGraveyard's: the cards are CHOSEN, so this prompts and is
    -- never an engine pick.
    --
    -- The ZONE is in the constructor rather than a field, ExileThisFromGraveyard's
    -- call above: Pawl.Engine.Cost.zoneOfComponent's CR 113.6m reading is a
    -- per-constructor match, so a cost naming a second zone is a second
    -- constructor rather than a parameter and no arm of that reading is ever
    -- unreachable. What THIS constructor's arm answers is Nothing, and that is a
    -- rules fact rather than an oversight -- see that function for why CR 113.6m
    -- does not reach a cost that moves cards other than the object it is on.
    --
    -- The Filter is matched against the PRINTED card and never a projection,
    -- which is the difference from Sacrifice's note above: nothing off the
    -- battlefield is projected (#160), so a graveyard candidate has only its
    -- printed characteristics to be matched on.
    --
    -- "YOUR graveyard", per CR 400.3 and CR 108.4: a graveyard is a per-player
    -- zone whose members are its OWNER's, so the candidates are the paying
    -- player's own. Whose graveyard is fixed by the constructor for the same
    -- reason the zone is, so a cost reading somebody else's would be a second
    -- constructor rather than a field here.
    ExileCardsFromGraveyard Natural.Natural (Filter.Filter keyword)
  deriving (Eq, Ord, Show)
