module Pawl.Types.CostComponent where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents

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
    -- environment, which a cost has no access to at CR 601.2f time. CR 601.2b's
    -- X is PayLifeX below rather than a Quantity here, for the reason that
    -- constructor gives.
    PayLife Natural.Natural
  | -- | CR 107.3a's X as an amount of LIFE: "pay X life", where the value is
    -- announced by the caster at CR 601.2b (Hatred). CR 118.4 sends a cost with
    -- an X in it to CR 107.3, and CR 601.2b's "a variable cost that will be paid
    -- as it's being cast (such as an {X} in its mana cost)" names the mana cost
    -- as an EXAMPLE, so an additional cost carrying X is announced by the same
    -- rule.
    --
    -- A SEPARATE CONSTRUCTOR and not a Quantity in PayLife above, for that
    -- constructor's stated reason: nothing here is evaluated against a binding
    -- environment. Pawl.Engine.Cost.substituteX rewrites this to a
    -- @PayLife n@ carrying the announced value, exactly as
    -- Pawl.Engine.Mana.substituteX rewrites a ManaSymbol.Variable, so every
    -- reader downstream of the announcement sees a number.
    --
    -- UNPAYABLE as it stands, which is Pawl.Engine.Cost.canPayComponent's answer:
    -- a value the caster has not announced is not one this cost can charge, and
    -- CR 601.2 reverses the casting rather than guessing. Nothing reaches payment
    -- carrying it, since Pawl.Engine.Cost.hasVariable reads the components and
    -- both cast paths substitute before they pay.
    PayLifeX
  | -- | CR 701.21a: sacrifice this many permanents matching the Filter (Village
    -- Rites' one creature, Fireblast's two Mountains). The player chooses which,
    -- so this is a prompt and never an engine pick.
    --
    -- The Filter is matched against the PROJECTION, never a printed
    -- characteristic: Blood Moon makes a nonbasic land a Mountain, and it may be
    -- sacrificed as one.
    Sacrifice (Sacrifice.Sacrifice keyword)
  | -- | CR 702.122a's cost half: tap ANY NUMBER of untapped permanents matching
    -- the Filter, chosen so that their TOTAL power reaches totalPower.
    -- Crew 6's "tap any number of other untapped creatures you control with
    -- total power 6 or greater" is the printing, and the whole of "other
    -- untapped creatures you control" rides the Filter rather than this
    -- constructor -- CR 702.122a's "other" is `Not IsSource`, its "untapped" is
    -- `Not IsTapped` and its "you control" is `ControlledBy You`, all three
    -- already in the vocabulary.
    --
    -- NOT `Sacrifice`-shaped, and the difference is the whole point of a second
    -- arm -- which is why the two payload records are separate despite holding
    -- the same two types: Sacrifice's count is HOW MANY objects, which the payer
    -- must match exactly, while totalPower is a THRESHOLD on an aggregate of the
    -- chosen set.
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
    TapForTotalPower (TapForTotalPower.TapForTotalPower keyword)
  | -- | Tap exactly this many permanents matching the Filter, chosen by the
    -- payer. Springleaf Drum's "{T}, Tap an untapped creature you control" is
    -- the printing, and the whole of "an untapped creature you control" rides
    -- the Filter -- "untapped" is `Not IsTapped`, "you control" is
    -- `ControlledBy You`, and a card printing "another" adds `Not IsSource`,
    -- all three already in the vocabulary. CR 601.2f names it outright --
    -- "costs may include paying mana, tapping permanents, sacrificing
    -- permanents, discarding cards, and so on" -- so unlike the exile
    -- components below this one needs no appeal to CR 118.1.
    --
    -- Sacrifice's shape (a count plus a criterion) rather than
    -- TapForTotalPower's, and that is the whole reason for a third tapping arm:
    -- CR 702.122a's threshold leaves the number of objects undetermined, so it
    -- can express neither "exactly one" nor any other count. Narrowing that
    -- component's Filter cannot close the gap either, since a Filter is a
    -- predicate over ONE candidate.
    --
    -- NOT TapThis with a criterion: that one names the object the cost is on
    -- and offers nothing to choose, which is SacrificeThis' distinction from
    -- Sacrifice drawn again.
    --
    -- CR 302.6 does NOT reach it, so it does not join TapThis and UntapThis in
    -- Pawl.Engine.Cost.requiresSicknessCheck -- see that function. The tapped
    -- permanent may have arrived this turn.
    TapPermanents (TapPermanents.TapPermanents keyword)
  | -- | CR 601.2f: discard this many cards matching the Filter from hand
    -- (Cathartic Reunion's two of any card, Magmatic Insight's one land card).
    -- CR 701.9b gives the choice to the discarding player, so this is a prompt
    -- and never an engine pick -- the same shape Sacrifice above has.
    --
    -- A count plus a criterion, Sacrifice's shape and
    -- ExileCardsFromGraveyard's: the cards are chosen, so the criterion narrows
    -- what may be offered rather than picking anything.
    --
    -- The Filter is matched against the card's own CR 613 projection,
    -- ExileCardsFromGraveyard's reading below and for its reason -- see
    -- Pawl.Engine.Cost.discardCandidates.
    -- DiscardThis below states the rest of what reading a hand costs, for both
    -- of the components that do.
    DiscardCards (DiscardCards.DiscardCards keyword)
  | -- | CR 702.29a's and CR 702.77a's "Discard this card": discard the card the
    -- cost is on, from the hand it is in. Two rules print the same clause, so
    -- the component carries the Pawl.Types.DiscardCause the payment logs -- CR
    -- 702.29c's "when you cycle this card" means "when you discard this card to
    -- pay an activation cost OF A CYCLING ABILITY", and rule 702.77 never says
    -- reinforce is one, where rule 702.29f says exactly that of typecycling.
    -- Whoever mints the component knows which rule it is spelling; the payment
    -- does not, so the cause travels with the component rather than being
    -- guessed at the pay site.
    --
    -- Deliberately NOT a one-card DiscardCards above, and the distinction is the
    -- one SacrificeThis draws against Sacrifice: CR 701.9b gives the discarding
    -- player the choice of WHICH card, so DiscardCards prompts -- while this
    -- names one card and offers nothing to choose. Paying with a DiscardCards
    -- would invent a prompt the rules do not have, and would let the player
    -- discard some other card to cycle this one. Narrowing that component's
    -- Filter cannot close the gap either: a criterion states a QUALITY, and no
    -- quality names one card (CR 201.2's name comes closest and two copies share
    -- it).
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
    DiscardThis DiscardCause.DiscardCause
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
  | -- | CR 118.12's counter-placing cost: put this many +1/+1 counters on the
    -- permanent the cost is on. CR 701.63a's endure -- "creates an N/N white
    -- Spirit creature token UNLESS THEY PUT N +1/+1 COUNTERS ON THAT PERMANENT"
    -- (Fortress Kin-Guard) -- is the printing, and CR 702.123a's fabricate prints
    -- the same shape spelled out. What makes the action a cost rather than an
    -- effect is CR 118.12a's rewriting plus CR 118.12's own sentence: "the action
    -- [do something] is a cost, paid when the spell or ability resolves".
    --
    -- +1/+1 SPECIFICALLY, PayEnergy's call: loyalty is the only other counter any
    -- cost puts on the object it is on, and AddLoyaltyToThis above already has
    -- it. A CounterKind payload would also pull a Keyword -- and through it a
    -- Filter -- into every traversal of this type, for a kind no card asks for.
    --
    -- RESOLUTION-TIME only, which Blight below is not, and that is what decides
    -- CR 614.16 -- see Pawl.Engine.Cost.payComponent for why this one goes
    -- through the counter funnel as an effect while AddLoyaltyToThis does not.
    --
    -- A Natural and not a Quantity, for PayLife's reason.
    PutPlusOneCountersOnThis Natural.Natural
  | -- | CR 701.68a as a COST: "blight N" -- the paying player puts N -1\/-1
    -- counters on a creature they control, choosing which as the cost is paid.
    -- CR 601.2f demands it as an additional cost to cast (Bogslither's Embrace),
    -- CR 602.1b as part of an activation cost (Dawnhand Dissident's "{T}, Blight
    -- 1:"), and CR 118.12 as a cost paid on resolution (Boggart Mischief's "you
    -- may blight 1. If you do, ...").
    --
    -- The one component paid at BOTH times, which every other one here is not:
    -- PutPlusOneCountersOnThis above is resolution-time alone and the rest are
    -- announcement-time alone. Pawl.Engine.Cost.payComponent says what that costs
    -- on CR 614.16.
    --
    -- NOT PutPlusOneCountersOnThis with a different kind, and the difference is
    -- not the counter: that component names the object the cost is ON and offers
    -- nothing to choose, where rule 701.68a names "a creature you control" and the
    -- payer picks one out of however many they have -- SacrificeThis's distinction
    -- from Sacrifice, drawn again.
    --
    -- UNPAYABLE when the payer controls no creature, which is CR 701.68b read for
    -- a cost: "if a player is given the choice to blight but is unable to put N
    -- -1\/-1 counters on a creature they control ... they can't choose to blight".
    -- Through Pawl.Engine.Cost.canPayComponent, so an activated ability with this
    -- cost is never OFFERED (CR 601.2h, reaching an activation through CR 602.2b)
    -- and CR 118.12's resolution offer is never raised (CR 118.3).
    --
    -- A Natural and not a Quantity, for PayLife's reason -- nothing here is
    -- evaluated against a binding environment. Not implemented: Soul Immolation's
    -- "blight X", whose X is announced at CR 601.2b under a bound rule 701.68a
    -- does not state, so it is neither this nor PayLifeX's shape (#1646).
    Blight Natural.Natural
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
    -- The Filter is matched against the graveyard card's own CR 613 projection
    -- (Pawl.Engine.Cost.exileCandidates), which rule 613.1 gives it exactly as it
    -- gives one to a permanent.
    --
    -- "YOUR graveyard", per CR 400.3 and CR 108.4: a graveyard is a per-player
    -- zone whose members are its OWNER's, so the candidates are the paying
    -- player's own. Whose graveyard is fixed by the constructor for the same
    -- reason the zone is, so a cost reading somebody else's would be a second
    -- constructor rather than a field here.
    ExileCardsFromGraveyard (ExileCardsFromGraveyard.ExileCardsFromGraveyard keyword)
  | -- | CR 406.2 a third time, in its FIXED form: exile the topmost card of the
    -- paying player's graveyard that matches the Filter. Circling Vultures'
    -- "unless you exile the top creature card of your graveyard" is the
    -- printing. A cost by CR 118.1's general reading, ExileCardsFromGraveyard's
    -- argument unchanged.
    --
    -- ExileThisFromGraveyard's side of the axis rather than
    -- ExileCardsFromGraveyard's, even though it names another card: CR 404.2
    -- keeps a graveyard in a fixed order that a player "normally can't change",
    -- so "the top creature card" identifies exactly one card and there is
    -- nothing to prompt for. Offering a choice here would be MORE permissive
    -- than the printing, which is why the general constructor cannot stand in.
    --
    -- No count. CR 404.1 gives a graveyard one top, and a card asking for two
    -- fixed cards would be asking about an order this constructor does not
    -- expose.
    --
    -- "The TOP" is the LAST member of Pawl.Engine.Game.zoneMembers' answer:
    -- CR 404.1 puts an arrival "on top of its owner's graveyard" and
    -- Pawl.Engine.Game.insertIntoZone appends it, so the most recent card is
    -- last. That is the opposite end from a library, whose head is its top.
    --
    -- The zone, the owner and the printed-not-projected matching are all
    -- ExileCardsFromGraveyard's, for its reasons.
    ExileTopFromGraveyard (Filter.Filter keyword)
  deriving (Eq, Ord, Show)
