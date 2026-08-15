module Pawl.Types.Quantity where

import qualified Pawl.Types.AgainstSlot as AgainstSlot
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Halved as Halved
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCount as ManaCount
import qualified Pawl.Types.PlayerCounterTally as PlayerCounterTally
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Plus as Plus
import qualified Pawl.Types.SlotName as SlotName

-- | A number that may not be a literal number.
--
-- Deliberately NOT called Characteristic: CR 109.3 already uses that word for an
-- object's whole characteristic set (name, mana cost, color, type line, rules
-- text, abilities, power, toughness, loyalty, defense, …). This is just a value.
--
-- Grows further: OneHalf (Little Girl's printed ½ power, which is a fractional
-- LITERAL and not the Halved arm below), Infinite (Mox Lotus). Plus is binary and
-- recursive so composition covers the awkward printed values without new cases:
-- 1+* is a Plus of Literal 1 and Star.
--
-- Deliberately NO Num instance. "Numeric tower" names the problem domain, not a
-- class hierarchy. Num would be lawless and partial once Star and Infinite exist
-- (signum Star? negate Infinite?), which collides with the no-partial-functions
-- rule, and fromInteger would silently erase the very distinction this type
-- exists to draw. Combining is explicit named functions.
data Quantity
  = Literal Integer
  | -- | CR 202.3: an object's mana value, computed from its mana cost. A
    -- computed quantity (Opalescence's "base P/T equal to its mana value"),
    -- evaluated against the affected object.
    ManaValue
  | -- | CR 208.1: the power of the object this quantity is evaluated against --
    -- for an effect, its SOURCE (Ghitu Fire-Eater's "damage equal to its
    -- power"). The projected value, not the printed one, so a pumped source
    -- deals what it currently has.
    --
    -- Sibling of ManaValue, and read the same way: against the `oid` the caller
    -- supplies, never against a target. Power is PROJECTED (CR 613 layer 7)
    -- where mana value is computed from the printed cost, so this arm goes
    -- through the injected ViewOf -- which is also what lets a caller substitute
    -- CR 608.2h last known information for a source that is gone.
    Power
  | -- | CR 208.1's other half: the toughness of the object this quantity is
    -- evaluated against. Power's sibling in every respect -- projected, read
    -- through the same injected ViewOf, naming no object of its own.
    --
    -- CR 702.100a's evolve is what asks: "that creature's power is greater than
    -- this creature's power and/or that creature's toughness is greater than this
    -- creature's toughness" is two comparisons, and only one of them is power's.
    Toughness
  | -- | A number an EARLIER effect of the same resolution bound into a slot --
    -- Bane of Progress' "for each permanent destroyed this way". Read from the
    -- binding environment of whichever object the evaluation is aimed at; for
    -- Pawl.Engine.Resolve, the effect's SOURCE, which is the same object the
    -- producing effect wrote to.
    --
    -- Named InSlot rather than Bound because PlayerRef.InSlot and
    -- ObjectRef.InSlot already spell "read the binding at this slot" that way.
    --
    -- CR 601.2b's X is THIS arm, at Pawl.Engine.Binding.variableX: #14 retired
    -- the separate constructor once the two were reading the identical field.
    -- What was special about X was never the reading but the LINTS around it --
    -- "reads X iff the cost declares {X}", and the one slot Quantity.slots must
    -- not report, since casting fills it and no card declares it. Those now name
    -- the reserved slot instead of a constructor, which is where the exception
    -- belonged: it is a fact about that slot, not about how a number is read.
    --
    -- Nothing when the slot holds no amount -- the producing effect has not run
    -- yet, or bound nothing. Not 0: "how many were destroyed" is unanswered, not
    -- answered with zero.
    InSlot SlotName.SlotName
  | -- | CR 208.2 / 208.2a: the star printed in a power/toughness box, standing for
    -- a value a characteristic-defining ability defines. NOTATION, not a value:
    -- Quantity.evaluate returns Nothing for it. Projection.baseCharacteristics
    -- resolves it at the seed by substituting Face.characteristicPT, so a Star
    -- is meant to never survive into a projection -- but that is a CONVENTION the
    -- card data must honour, not something this type guarantees. Two cases reach
    -- the evaluator anyway, both benign:
    --
    --   * a card whose OWN characteristicPT itself contained a Star. The seed
    --     substitutes without re-descending into the replacement, so the star
    --     reaches the CDA's evaluator and CR 208.2a makes the undeterminable
    --     value 0. Hypothetical: no card in the pool does this.
    --   * a card with printed Star power/toughness and NO characteristicPT at
    --     all, so there is nothing for the seed to substitute and the Star
    --     reaches evaluate directly, where CR 208.2a's substitution does not
    --     apply (there is no CDA). Real: Primal Plasma, whose star gets its value
    --     from an as-enters REPLACEMENT (CR 208.2b). On the battlefield that
    --     replacement has always run before anything reads the power, so the
    --     evaluator sees a number; OFF it, CR 208.2b's last sentence makes the
    --     answer 0, and Projection.printedPower is where that 0 is written --
    --     deliberately not here, since this arm answers for the projection seed,
    --     where a surviving star is a hole rather than a zero.
    Star
  | -- | CR 208.2: composition, so a printed 1+* needs no constructor of its own.
    Plus (Plus.Plus Quantity)
  | -- | CR 107.1a: half the inner quantity, rounded the way the card prints --
    -- Malignus' "half the highest life total among your opponents, rounded up",
    -- Aspect of Wolf's "half the number of Forests you control, rounded down".
    --
    -- The DIVISOR is in the constructor and the DIRECTION is a payload, which is
    -- what the printed text distinguishes: every card that halves says which way
    -- it rounds, and Aspect of Wolf says both ways in one sentence, so no engine
    -- rule could pick. See Pawl.Types.Rounding.
    --
    -- Two rather than a general divisor, for BlockersBeyondFirst's reason: only
    -- "half" is printed on a card that computes a value this way. "Divided
    -- evenly" (Fireball) is a division among TARGETS rather than of a value, and
    -- would be an announcement (CR 601.2d) rather than a quantity.
    --
    -- Not a leaf: the payload is a whole Quantity, so composition reaches
    -- everything the type can read -- half a count, half a life total, half a
    -- slot's amount.
    Halved (Halved.Halved Quantity)
  | -- | The negation of the quantity inside it -- Toxic Deluge's "all creatures
    -- get -X/-X until end of turn", where the minus sign is in front of a value
    -- the card does not print.
    --
    -- Literal already takes a signed Integer, so a printed -5/-5 (Dismember) is
    -- writable without this; what is not is a minus in front of anything else,
    -- since Plus is the only other arithmetic arm and this type has no inverse.
    -- The sign belongs to the CARD, never to the announcement: CR 107.3a has the
    -- controller announce X, and CR 107.1b forbids choosing a negative number,
    -- so "-X/-X" is a minus the text prints in front of a nonnegative value.
    --
    -- A negative result is legal where it lands. CR 107.1b: "it's possible for a
    -- game value, such as a creature's power, to be less than zero", and
    -- Pawl.Engine.Projection.addPT does not floor -- CR 704.5f's state-based
    -- action is what a toughness of 0 or less means. Where the reader wants a
    -- COUNT instead, CR 107.1b's other half ("if a calculation ... yields a
    -- negative number, zero is used instead") is already applied by
    -- Pawl.Extra.Integer.toNaturalSaturating at every such reader, so a negated
    -- draw or token count is 0 rather than an error.
    --
    -- NOT a product node. Pawl.Engine.Keyword.rampage rejected one for its "N
    -- times the number of creatures blocking it", on the grounds that it would be
    -- a second way to write numbers Plus already writes; -1 is the only
    -- coefficient no composition of Plus can reach, so it is the only one that
    -- earns an arm. NOT a Minus either: Negate composes with Plus into the
    -- subtraction a card prints, where a Minus arm would leave the negation of a
    -- single value spelled as a subtraction from zero. The Ten Rings' "draw cards
    -- equal to the difference" is that composition -- Plus of Literal 10 and a
    -- negated hand count.
    --
    -- Not a leaf: the payload is a whole Quantity, so every fold recurses through
    -- it as it does through Plus, and it terminates for Plus's reason -- the
    -- payload is a strictly smaller subterm.
    --
    -- Nor is this the Num instance the header rules out: a SYNTAX node, whose
    -- evaluation is unanswered exactly where its payload's is (the negation of a
    -- bare Star is not 0), where a Num method would have to answer everywhere.
    Negate Quantity
  | -- | A quantity that counts game state (CR 208.2a, CR 608.2h). See
    -- Pawl.Types.Count for why the payload is its own type.
    --
    -- This is where the knot is tied: a Count's Aggregation may itself name a
    -- per-object Quantity (Aggregation.Greatest), so the payload is
    -- `Count Quantity` and the recursion lives in the DATA rather than in a
    -- module cycle. Structural, not a recursive call -- evaluating a Greatest
    -- descends into a strictly smaller Quantity, so the fold terminates.
    Count (Count.Count Quantity)
  | -- | A quantity that counts a MANA POOL (CR 106.4) -- Omnath, Locus of Mana's
    -- "for each unspent green mana you have". See Pawl.Types.ManaCount for why
    -- the pool cannot be a Count's Scope and why the payload is its own type
    -- rather than a second shape of Count.
    --
    -- Not part of the knot Count above ties: a ManaCount holds no Quantity, so
    -- this arm is a LEAF and the recursion this type has does not reach it.
    ManaCount ManaCount.ManaCount
  | -- | CR 119.1: a player's life total -- Serra Avatar's "equal to your life
    -- total". A player's own scalar, which is why it is NOT a Count: CR 400.1
    -- scopes a Count over a ZONE and its Aggregation folds over the objects
    -- there, while a life total is one number attached to a player and no
    -- population at all.
    --
    -- The PlayerRef says WHOSE, the same payload Effect.LoseLife and
    -- Effect.GainLife already carry to say whose life changes; this is the read
    -- direction of that pair. CR 109.5's "you" is PlayerRef.Relative
    -- PlayerRelation.You, resolved against the evaluation context -- for a
    -- characteristic-defining ability that is the object's OWN controller (CR
    -- 604.3a(3)), which is what makes Serra Avatar track its controller's life
    -- rather than its source's.
    --
    -- A LEAF, like ManaCount above and unlike Count: it holds no Quantity, so
    -- the recursion this type has does not reach it.
    LifeTotal PlayerRef.PlayerRef
  | -- | CR 702.179: a player's speed -- what CR 702.179e's "max speed" and CR
    -- 702.178a's "as long as your speed is 4" ask about. LifeTotal's sibling in
    -- every respect: one scalar attached to a player rather than a population in
    -- a zone, so it is not a Count, and it carries the same PlayerRef to say
    -- whose.
    --
    -- CR 702.179f is applied by the EVALUATOR, not by this type: a player with no
    -- speed at all (Player.speed of Nothing, CR 702.179b) evaluates to 0 here,
    -- because the rule says their speed IS 0 "for the purpose of an effect that
    -- refers to speed" -- and this constructor is exactly such an effect's
    -- reading. Nothing would be "unanswered", which is a different claim and the
    -- wrong one.
    --
    -- A LEAF, like LifeTotal and ManaCount: it holds no Quantity.
    Speed PlayerRef.PlayerRef
  | -- | CR 725.1: is that player the monarch? 1 if so and 0 if not -- Dawnglade
    -- Regent's "as long as you're the monarch". A DESIGNATION read as a number,
    -- which is what lets Pawl.Types.Condition ask about it at all: that type's
    -- vocabulary is numeric and has exactly one constructor, and Kird Ape's "as
    -- long as you control a Forest" is already a count compared against 1, so a
    -- yes/no is the same comparison with a 0/1 on the measured side.
    --
    -- LifeTotal's and Speed's sibling: one fact attached to a player rather than
    -- a population in a zone, so it is not a Count -- CR 400.1 scopes a Count
    -- over a zone and its Aggregation folds over the objects there, and CR 725.1
    -- makes the monarch a game-wide player designation with no object and no
    -- zone. Nor is it a Filter: a Filter is asked per CANDIDATE OBJECT, and there
    -- is no object here to ask about.
    --
    -- NO MONARCH AT ALL evaluates to 0, not to Nothing, and CR 725.5 is the rule
    -- that says so: "if the result of a continuous effect generated by a static
    -- ability is determined based on who is currently the monarch, but there is
    -- no monarch in the game as that effect begins to apply, that effect does
    -- nothing until a player becomes the monarch". The rule gives the answer, so
    -- the answer is not unanswered -- the same argument Speed above makes for CR
    -- 702.179f's absent speed. Nothing would claim the question could not be put,
    -- which is a different and wrong claim.
    --
    -- A LEAF, like LifeTotal, Speed and ManaCount: it holds no Quantity.
    IsMonarch PlayerRef.PlayerRef
  | -- | CR 122.1: how many counters of a kind a PLAYER has -- CR 728.1's "a
    -- number of cards equal to the number of rad counters they have", and the
    -- shape Ezuri, Claw of Progress' "where X is the number of experience
    -- counters you have" asks for.
    --
    -- LifeTotal's and Speed's sibling: one number attached to a player rather
    -- than a population in a zone, so it is not a Count -- CR 400.1 scopes a
    -- Count over a zone and its Aggregation folds over the objects there, and a
    -- player's counter store is neither. The PlayerRef says whose, the
    -- PlayerCounterKind which.
    --
    -- A counter kind ABSENT from Player.counters reads as 0 rather than as
    -- unanswered, which is that field's own stated convention: a player who has
    -- never been given a rad counter has no rad counters, which is a number.
    --
    -- The OBJECT-counter reading (CR 122.1a-e) is ObjectCounters below, which is
    -- a different arm because it must say which OBJECT where this says which
    -- player.
    --
    -- A LEAF, like LifeTotal, Speed and ManaCount: it holds no Quantity.
    PlayerCounters PlayerCounterTally.PlayerCounterTally
  | -- | CR 122.1: how many counters of a kind are on the OBJECT this quantity is
    -- evaluated against -- Promising Duskmage's "if it had a +1/+1 counter on
    -- it".
    --
    -- Power's sibling rather than PlayerCounters', and for Power's reason: it
    -- reads the `oid` the caller supplies and is answered through the injected
    -- ViewOf, which is what lets a caller substitute CR 608.2h last known
    -- information for an object that is gone. That substitution is the whole
    -- point here -- CR 613.4c folds a +1/+1 counter into layer 7c's power and
    -- toughness, so a projection taken as the object died records the counter's
    -- EFFECT and never the counter, and only a record of the counters themselves
    -- can answer this.
    --
    -- No ObjectRef beside it: like Power, it names no object at all and takes the
    -- one the evaluation is aimed at. Reading another object's counters would be
    -- a Count over a zone, which is a different shape and one nothing in the pool
    -- asks for.
    --
    -- A kind ABSENT from the object's counters reads as 0 rather than as
    -- unanswered, exactly as PlayerCounters above: an object nobody put a counter
    -- on has none, which is a number. Nothing survives only for an object the
    -- view cannot describe at all.
    --
    -- A LEAF: it holds no Quantity.
    ObjectCounters (CounterKind.CounterKind Keyword.Keyword)
  | -- | Does the OBJECT this quantity is evaluated against have this designation?
    -- 1 if so and 0 if not -- rule 702.112a's "if it isn't renowned", which CR
    -- 603.4 makes an intervening "if" and so a Pawl.Types.Condition; CR 701.37a's
    -- "if this permanent isn't monstrous" and Repeat Offender's "if this creature
    -- is suspected", which are the same test carried as an activated ability's
    -- clause condition rather than as an intervening "if".
    --
    -- IsMonarch's shape with the two sides swapped, and ObjectCounters' position:
    -- a DESIGNATION read as a number, because Condition's vocabulary is numeric,
    -- but one that rides an OBJECT rather than a player. So it takes no reference
    -- at all -- like Power and ObjectCounters it reads the object the evaluation is
    -- aimed at, through the same injected ViewOf, which is what lets CR 608.2h last
    -- known information answer for a creature that is gone when the ability
    -- resolves. Reaching another permanent's designation is AgainstSlot's job, not
    -- a second constructor's.
    --
    -- NOT the Filter atom, which is Pawl.Types.Filter's own HasDesignation: rule
    -- 702.112a asks about the ability's own bearer, which is Power's position and
    -- not a candidate's. "A renowned creature you control" (Aragorn, Hornburg
    -- Hero) and "a suspected creature" (Rune-Brand Juggler) are the candidate
    -- reading, and ask the same designation of the other side.
    --
    -- A LEAF: it holds no Quantity.
    HasDesignation Designation.Designation
  | -- | CR 702.33d: was the SPELL this quantity is evaluated against kicked? 1 if
    -- so and 0 if not -- Burst Lightning's "if this spell was kicked", the clause
    -- condition rule 702.33e makes an ability of its own.
    --
    -- HasDesignation in every structural respect above, and a different KIND of
    -- fact: that atom reads a mark on a permanent, and this is a record of a choice
    -- its controller made as the spell was cast (CR 601.2b). What makes it the same
    -- shape is the reader -- one object, one Bool, off the view.
    --
    -- No Filter atom beside it: "target spell that was kicked" is text no card in
    -- the pool prints, kicker's payoff always being an ability of the kicked spell
    -- itself.
    --
    -- A LEAF: it holds no Quantity.
    WasKicked
  | -- | CR 508.3b: how many of that player's opponents were DECLARED attacked
    -- this combat phase -- rule 702.121a's "for each opponent you attacked with a
    -- creature this combat".
    --
    -- LifeTotal's and Speed's sibling in shape: one number attached to a player,
    -- so it is not a Count -- CR 400.1 scopes a Count over a zone, and the combat
    -- record is not one. What it reads is Pawl.Types.Combat.declaredAttacked,
    -- which is exactly the rule's question already: that field exists because CR
    -- 508.4 says a creature put onto the battlefield attacking never "attacked",
    -- and melee's own ruling repeats it.
    --
    -- DECLARED targets rather than live attackers, and the same field rather than
    -- Combat.attacked, is what makes the three printed readings come out right: a
    -- creature that has left combat still counts, a creature that entered
    -- attacking never does, and an opponent who has since left the game counts
    -- too -- nothing here asks who is still playing.
    --
    -- Only OfPlayer entries count. CR 506.3 lets a creature attack a planeswalker
    -- or a battle, and rule 702.121a counts OPPONENTS, so attacking an opponent's
    -- planeswalker and nothing else is a bonus of 0.
    --
    -- The PlayerRef says whose opponents, CR 109.5's "you" being
    -- PlayerRef.Relative PlayerRelation.You. WHO attacked is not recorded and does
    -- not need to be: CR 506.2 makes the attacking player the active player, so
    -- one combat phase's record can only be that player's attacks (#175 is where
    -- CR 802 would break that).
    --
    -- A LEAF, like LifeTotal, Speed and IsMonarch: it holds no Quantity.
    OpponentsAttacked PlayerRef.PlayerRef
  | -- | CR 701.9a / 608.2i: how many cards that player has DISCARDED this turn --
    -- what Asmoranomardicadaistinaculdacar's "as long as you've discarded a card
    -- this turn" asks, compared against 1.
    --
    -- LifeTotal's and OpponentsAttacked's sibling in shape: one number attached to
    -- a player rather than a population in a zone, so it is not a Count -- CR 400.1
    -- scopes a Count over a zone and the event log is not one.
    --
    -- A LOOK-BACK read of the turn-scoped GameEvent log, which CR 608.2i sanctions,
    -- and "this turn" is the log's own extent rather than a window named here --
    -- the footing Filter.AttackedThisTurn already stands on. It counts
    -- GameEvent.Discarded and not a hand-to-graveyard zone change, the two being
    -- different questions in both directions: CR 701.1 and CR 701.9a make
    -- discarding the keyword ACTION of moving a card from a hand to a graveyard, so
    -- a move no effect performed as a discard is not one, and CR 701.9c leaves a
    -- discarded card a replacement sent elsewhere still discarded.
    --
    -- NOT a Scope.InHistory count of GameEvent.Discarded events (#162): that arm
    -- matches a Filter against the event's characteristic snapshot, and a Discarded
    -- event carries none -- there would be nothing for the Filter to look at.
    --
    -- CR 702.29a's cycling is a discard too, so both DiscardCause values count. An
    -- EMPTY log answers 0 rather than Nothing, as OpponentsAttacked's empty record
    -- does: nobody having discarded is a number.
    --
    -- A LEAF, like LifeTotal, Speed and OpponentsAttacked: it holds no Quantity.
    CardsDiscardedThisTurn PlayerRef.PlayerRef
  | -- | CR 120.1 / 608.2i: how many of the players this reference names WERE DEALT
    -- DAMAGE this turn -- Furious Spinesplitter's "for each opponent who was dealt
    -- damage this turn", and the measurement rule 702.54a's bloodthirst compares
    -- against 1.
    --
    -- CardsDiscardedThisTurn's sibling in footing and IsMonarch's in ARITY. It is a
    -- look-back read of the turn-scoped GameEvent log that CR 608.2i sanctions, with
    -- "this turn" the log's own extent rather than a window named here; but unlike
    -- the discard tally it answers for a reference naming ANY number of players,
    -- because the question is per-player and the card asks how many of them it holds
    -- of. So "an opponent was dealt damage" and "each opponent who was dealt damage"
    -- are the same measurement read at two thresholds.
    --
    -- PLAYERS and not EVENTS. Two Lightning Bolts at one opponent is one opponent,
    -- which is what "for each opponent who" counts; a tally of DamageDealt events
    -- would say two. Damage to a planeswalker or a battle that player controls is
    -- not damage to the player either (CR 120.3a names the player recipient alone),
    -- and neither is damage to their creatures.
    --
    -- NOT a Scope.InHistory count over an EventShape arm for damage, which is what
    -- this measurement was expected to need; see #1511. Two things rule it out and
    -- neither is incidental: an InHistory
    -- fold's members are Filter views of an OBJECT, and CR 120.3a's recipient here
    -- is a player, who has no view; and the fold counts EVENTS, which is the wrong
    -- unit per the paragraph above.
    --
    -- An EMPTY log answers 0 rather than Nothing, as CardsDiscardedThisTurn's does:
    -- nobody having been dealt damage is a number. Nothing is reserved for a
    -- reference that could not be resolved at all.
    --
    -- A LEAF, like LifeTotal, Speed and CardsDiscardedThisTurn: it holds no Quantity.
    PlayersDealtDamageThisTurn PlayerRef.PlayerRef
  | -- | CR 400.7 / 608.2i: did the object this quantity is evaluated against ENTER
    -- THE BATTLEFIELD this turn? 1 if so and 0 if not -- Thrasta, Tempest's Roar's
    -- "Thrasta has hexproof as long as it entered this turn", a CR 604.1 static
    -- ability's CR 604.2 clause rather than a trigger. The battlefield and no other
    -- zone, because that is what a bare "enters" abbreviates (CR glossary, "enters
    -- the battlefield").
    --
    -- BlockersBeyondFirst's shape rather than CardsDiscardedThisTurn's: it names no
    -- object and takes the one the evaluation is aimed at, so a card that wants
    -- another permanent's answer reaches it through AgainstSlot. Read LIVE off
    -- GameState.events rather than through the injected view -- an entry is an event
    -- and not a characteristic, so no projection can answer it.
    --
    -- "This turn" is the LOG'S OWN EXTENT and not a window named here, exactly as
    -- CardsDiscardedThisTurn has it: Engine.beginTurnOf clears the log at the turn
    -- handoff, so an entry still in it is an entry this turn.
    --
    -- Matched against the same GameEvent.Moved-to-the-battlefield test CR 603.6a's
    -- PermanentEnters trigger uses, keyed on ZoneChange.object -- the RESULTING
    -- incarnation's id, which is the id the permanent now has. CR 400.7 is what
    -- makes that the whole of the question: a permanent that left and came back is
    -- a new object, and it entered when the new object did.
    --
    -- NOT a Designation and NOT a stored flag on the object. An entry is already
    -- recorded, and a flag would be a second writer of the same fact -- the stale-read
    -- shape a derived condition with a stored consumer keeps producing here.
    --
    -- Nor is it summoning sickness (CR 302.6), which asks a DIFFERENT question:
    -- "under that player's control since their most recent turn began" survives a
    -- turn handoff on an opponent's turn, where this does not.
    --
    -- An EMPTY log answers 0 rather than Nothing, as CardsDiscardedThisTurn's does:
    -- a permanent that did not enter this turn is an answered question. Nothing
    -- survives only for an evaluation aimed at no object at all.
    --
    -- A LEAF: it holds no Quantity.
    EnteredThisTurn
  | -- | CR 509.1h / 702.23a: how many creatures are blocking the object this
    -- quantity is evaluated against, BEYOND THE FIRST -- rampage's "for each
    -- creature blocking it beyond the first".
    --
    -- Power's and ObjectCounters' sibling in shape: it names no object and takes
    -- the one the evaluation is aimed at. Unlike those two it is answered from
    -- Pawl.Types.Combat.blockers rather than through the injected view, combat
    -- being game state rather than a characteristic, so an object the view can no
    -- longer describe still answers as long as the declaration stands.
    --
    -- "Beyond the first" is IN the constructor rather than left to arithmetic.
    -- This type has Plus and no inverse, and a subtraction node written for it
    -- would have to floor at zero anyway: an unblocked object is blocked by no
    -- creatures, and rule 702.23a's phrase reads 0 there, not -1.
    --
    -- NOT a Count over the battlefield. A Count is scoped to a zone and matched
    -- by a Filter (CR 400.1), and "blocking THIS object" is a relation between two
    -- objects that no Filter atom states.
    --
    -- Unblocked reads 0 rather than Nothing, as PlayerCounters and ObjectCounters
    -- read an absent kind: nobody blocking is a number. Nothing survives only for
    -- an evaluation aimed at no object at all -- a member of an
    -- Aggregation.Greatest over Scope.InHistory, which describes a past event
    -- rather than anything in combat now.
    --
    -- A LEAF: it holds no Quantity.
    BlockersBeyondFirst
  | -- | Read the inner quantity against the OBJECT A SLOT NAMES rather than
    -- against the effect's source (CR 113.7) -- Soul's Majesty's "cards equal to
    -- the power of target creature you control", where the power read is the
    -- target's.
    --
    -- The one arm that MOVES the object every other object-reading arm (Power,
    -- ManaValue, ObjectCounters, HasDesignation, BlockersBeyondFirst) is
    -- aimed at.
    -- Those arms deliberately name no object, so this is the only way a card can
    -- say which one; without it a payload can read only its own source.
    --
    -- NOT InSlot, which reads an AMOUNT an earlier effect bound at a slot. This
    -- names a TARGET slot and reads a characteristic off what it points at, so
    -- the payload is a whole Quantity rather than nothing.
    --
    -- CR 109.5's "you" is untouched: the perspective stays the resolving
    -- controller's, so a Count inside still means the controller's permanents and
    -- not the target controller's.
    --
    -- Nothing when the slot names no object -- a player recipient, a slot the
    -- resolution never filled, an illegal target (CR 608.2b), or any evaluation
    -- outside a resolution, where there are no slots at all.
    AgainstSlot (AgainstSlot.AgainstSlot Quantity)
  deriving (Eq, Ord, Show)
