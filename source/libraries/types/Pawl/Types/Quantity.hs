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
  | -- | CR 103.1: is that player the starting player? 1 if so and 0 if not --
    -- Gemstone Caverns' "you're not the starting player", which is that card's
    -- opening-hand action's own gate. IsMonarch's shape exactly, and for the same
    -- reason: this type's vocabulary is numeric, so a yes/no is a comparison
    -- against a 0/1 on the measured side.
    --
    -- CR 103.1's last sentence is what makes the answer readable at all -- "the
    -- game's default turn order begins with the starting player" -- so the
    -- starting player is the head of GameState.turnOrder, which is that seating
    -- order rotated. A restarted game (CR 727.1a) and a subgame (CR 729.2) seat
    -- their own starting player, so each answers for itself.
    --
    -- Not implemented: CR 103.1c's Power Play, which makes its controller the
    -- starting player after the determination. That card is not in
    -- @data\/cards\/@ and there is no effect that reseats a turn order, so the
    -- head of the roster is the whole answer today (#1923).
    --
    -- A LEAF, like LifeTotal, Speed and IsMonarch: it holds no Quantity.
    IsStartingPlayer PlayerRef.PlayerRef
  | -- | CR 102.1: is that player the ACTIVE player -- the one whose turn it is? 1
    -- if so and 0 if not. Paladin Class' "spells your opponents cast during your
    -- turn cost {1} more to cast", where the taxing ability's own controller is
    -- the seat asked about. IsMonarch's and IsStartingPlayer's shape exactly, and
    -- for the same reason: this type's vocabulary is numeric, so a yes/no is a
    -- comparison against a 0/1 on the measured side.
    --
    -- "During your turn" and "is the active player" are the same question, which
    -- is CR 102.1's own wording -- the rule defines the active player AS the
    -- player whose turn it is, so a clause naming the turn and one naming the
    -- seat cannot disagree.
    --
    -- GameState.activePlayer is the whole answer: there is always exactly one, so
    -- a disjunction over the named seats and a sum over them agree on every
    -- board, and no arm here can be unanswerable for want of an active player the
    -- way IsMonarch can be for want of a monarch.
    --
    -- A LEAF, like LifeTotal, Speed, IsMonarch and IsStartingPlayer: it holds no
    -- Quantity.
    IsActivePlayer PlayerRef.PlayerRef
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
  | -- | CR 122.1: how many counters of EVERY kind are on the OBJECT this quantity
    -- is evaluated against, summed -- Savanti Romero, Time's Exile's "where X is
    -- the number of counters on Savanti Romero", and the Boolean reading of the
    -- same sum in Angelic Sleuth's "if it had counters on it".
    --
    -- ObjectCounters above in every respect but the kind: the same object (the one
    -- the evaluation is aimed at), the same injected ViewOf, and so the same CR
    -- 608.2h substitution for an object that is gone.
    --
    -- A SEPARATE ARM rather than a Maybe inside ObjectCounters' payload. Two
    -- reasons. The narrower question is not this one with a filter applied -- CR
    -- 122.1a-j give each kind its own rule, and a card asking after +1/+1 counters
    -- is asking about power and toughness where a card asking after counters is
    -- asking about CR 122.2's marker -- so an absent payload standing for "every
    -- kind" would make the field's absence the way a reader tells the two
    -- questions apart, which is the untagged union Pawl.Types.PlayerQuantity's
    -- haddock forbids. And every existing card's wire form stays exactly as
    -- printed, ObjectCounters' payload remaining required.
    --
    -- An object with NO counters reads as 0 rather than as unanswered, ObjectCounters'
    -- convention: an empty sum is a number. Nothing survives only for an object the
    -- view cannot describe at all.
    --
    -- A LEAF: it holds no Quantity.
    ObjectCountersOfAnyKind
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
  | -- | CR 716.2b / CR 716.2d: the LEVEL of the object this quantity is evaluated
    -- against -- CR 716.2a's "activate only if this Class is level N-1", carried
    -- as an activated ability's clause condition, and its "as long as this Class
    -- is level N or greater", carried as a static ability's CR 604.2 clause.
    --
    -- HasDesignation's position exactly, with a number where that has a Bool: rule
    -- 716.2b says a level IS a designation, so this reads a mark rather than a
    -- characteristic, and it names no object at all -- like Power and
    -- ObjectCounters it takes the one the evaluation is aimed at, through the same
    -- injected ViewOf. Reaching another permanent's level is AgainstSlot's job.
    --
    -- NOT ObjectCounters over CounterKind.Level, and CR 716.4 is why: a leveler's
    -- level counters and a Class's level are different marks that the rules
    -- explicitly keep from interacting.
    --
    -- An object with NO level reads as 1 rather than as 0 or as unanswered, which
    -- is CR 716.2d in the one place it can be enforced for every asker
    -- (Pawl.Types.ClassLevel.defaulted). Nothing survives only for an object the
    -- view cannot describe at all, as ObjectCounters above.
    --
    -- A LEAF: it holds no Quantity.
    ClassLevel
  | -- | CR 702.33d: was the spell this quantity is evaluated against kicked? 1 if
    -- so and 0 if not -- Burst Lightning's "if this spell was kicked", the clause
    -- condition rule 702.33e makes an ability of its own.
    --
    -- Asked of a PERMANENT as readily as of a spell, which is CR 400.7d: Monstrous
    -- War-Leech's payoff is a CR 614.1c entry replacement, so what it asks about is
    -- the spell that became the permanent it is asked of (see Pawl.Types.Object's
    -- `kicked`).
    --
    -- HasDesignation in every structural respect above, and a different KIND of
    -- fact: that atom reads a mark on a permanent, and this is a record of a choice
    -- its controller made as the spell was cast (CR 601.2b). What makes it the same
    -- shape is the reader -- one object, one Bool, off the view.
    --
    -- No Filter atom beside it: "target spell that was kicked" is text no card in
    -- the pool prints, kicker's payoff always being an ability of the kicked object
    -- itself.
    --
    -- A LEAF: it holds no Quantity.
    WasKicked
  | -- | CR 107.4h's third sentence: was any mana produced by a snow source spent
    -- to cast the spell this quantity is evaluated against? 1 if so and 0 if not
    -- -- Berg Strider's "if {S} was spent to cast this spell".
    --
    -- WasKicked above in every structural respect, CR 400.7d included: what the
    -- permanent's own triggered ability asks about is the spell that became it,
    -- and Pawl.Types.Object's `manaSpent` is where that record is kept.
    --
    -- CARRIES NOTHING, rather than naming which production tag it asks about:
    -- Pawl.Types.ProductionTag has one constructor, so the payload would be a
    -- choice with a single member. It grows into one when a second tag gets a
    -- retrospective reading of its own.
    --
    -- Not the spent mana's COLOUR. Boreal Outrider's "if {S} of any of that
    -- spell's colors was spent to cast it" does ask that, and it asks it about
    -- ANOTHER spell rather than about itself, so it wants two things this atom
    -- has not got (#2008).
    --
    -- A LEAF: it holds no Quantity.
    SnowWasSpent
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
    -- because the question is asked of each of them separately and the card wants
    -- how many said yes. So "an opponent was dealt damage" and "each opponent who
    -- was dealt damage" are one measurement read at two thresholds.
    --
    -- PLAYERS and not EVENTS. Two Lightning Bolts at one opponent is one opponent,
    -- which is what "for each opponent who" counts; a tally of DamageDealt events
    -- would say two. Damage to a planeswalker or a battle that player controls is
    -- not damage to the player either (CR 120.3a names the player recipient alone),
    -- and neither is damage to their creatures.
    --
    -- NOT a Scope.InHistory count over an EventShape arm for damage, which is what
    -- this measurement was expected to need; see #1511. Two things rule that out
    -- and neither is incidental: an InHistory fold's members are Filter views of an
    -- OBJECT, and CR 120.3a's recipient here is a player, who has no view; and the
    -- fold counts EVENTS, which is the wrong unit per the paragraph above.
    --
    -- An EMPTY log answers 0 rather than Nothing, as CardsDiscardedThisTurn's does:
    -- nobody having been dealt damage is a number. Nothing is reserved for a
    -- reference that could not be resolved at all.
    --
    -- Rule 702.54b's variant asks a DIFFERENT question -- "the total damage your
    -- opponents have been dealt this turn", which sums amounts where this counts
    -- players. Not implemented: no quantity measures that sum, and
    -- Keyword.Bloodthirst's payload is a printed Natural that cannot say X
    -- (#1588). The keyword itself IS implemented -- rule 702.54a's N form, whose
    -- condition is this quantity (Bloodrage Vampire).
    --
    -- A LEAF, like LifeTotal, Speed and CardsDiscardedThisTurn: it holds no Quantity.
    PlayersDealtDamageThisTurn PlayerRef.PlayerRef
  | -- | CR 601.2i / 608.2i: how many spells that player cast during the turn just
    -- ended -- Daybreak Ranger's "if no spells were cast last turn" and Nightfall
    -- Predator's "if a player cast two or more spells last turn".
    --
    -- LifeTotal's shape and not PlayersDealtDamageThisTurn's: this is ONE player's
    -- tally, so a reference naming several answers "whose?" rather than a sum. Both
    -- of the printed readings above are about every player at once, and both are
    -- spelled the way Malignus spells "the highest life total among your opponents"
    -- -- Aggregation.Greatest over Scope.OverPlayers, with this arm reading each
    -- candidate through PlayerRef.Candidate. That is what makes "no spells were
    -- cast" (greatest == 0) and "a player cast two or more" (greatest >= 2)
    -- different thresholds on one measurement, and it is why neither is a sum: two
    -- players casting one spell each is not a player casting two.
    --
    -- LAST turn and not this one, which is why it reads GameState.castsLastTurn
    -- rather than folding GameState.events as CardsDiscardedThisTurn does: the log
    -- is cleared at the handoff (Engine.beginTurnOf) and every reader of this is in
    -- a later turn, so the snapshot is the only surviving record. "This turn" is a
    -- different measurement over the live log, wanted by Brightspear Zealot and
    -- Ertai's Scorn. Not implemented: no arm measures it (#2185).
    --
    -- NOT GameState.spellsCastLastTurn, which is CR 502.2's scalar about the
    -- previous turn's ACTIVE PLAYER alone. Reading that here would answer 0 on a
    -- turn where only a nonactive player cast.
    --
    -- An ABSENT entry answers 0 rather than Nothing, as CardsDiscardedThisTurn's
    -- empty log does: nobody having cast is an answered question. Nothing is
    -- reserved for a reference that could not be resolved at all.
    --
    -- A LEAF, like LifeTotal and CardsDiscardedThisTurn: it holds no Quantity.
    SpellsCastLastTurn PlayerRef.PlayerRef
  | -- | CR 309.7: how many dungeons that player has completed -- Gloom Stalker's
    -- "as long as you've completed a dungeon", which is this compared to 1.
    --
    -- LifeTotal's shape and not PlayersDealtDamageThisTurn's: this is ONE player's
    -- tally, so a reference naming several answers "whose?" rather than a sum.
    -- Every printing that reads it says "you", so the reference is
    -- PlayerRef.Relative You and the arity never bites; the aggregate shape, if a
    -- card ever wants one, is Aggregation.Greatest over Scope.OverPlayers reading
    -- each candidate through PlayerRef.Candidate, as SpellsCastLastTurn above does.
    --
    -- A COUNT and not a set of dungeon names: CR 309.7 states only the fact of
    -- completion, and Player.completedDungeons argues why the names wait for
    -- Acererak the Archlich. Not implemented: the named read (#2259).
    --
    -- Read straight off Player.completedDungeons rather than folded from
    -- GameState.events, for the reason LifeTotal is read off the player: nothing
    -- logs the removal, and the log is cleared at every turn handoff while
    -- completion outlives the game's turns.
    --
    -- An ABSENT player answers 0 rather than Nothing, as SpellsCastLastTurn's
    -- absent entry does: nobody having completed a dungeon is an answered
    -- question. Nothing is reserved for a reference that could not be resolved.
    --
    -- A LEAF, like LifeTotal and SpellsCastLastTurn: it holds no Quantity.
    DungeonsCompleted PlayerRef.PlayerRef
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
