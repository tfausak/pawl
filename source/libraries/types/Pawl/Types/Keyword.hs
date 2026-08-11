module Pawl.Types.Keyword where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.MorphVariant as MorphVariant

-- | CR 702. A keyword is a CITATION, not an effect: rule 702 is part of the
-- comprehensive rules, the same as rule 506 or rule 302. So casing on this is NOT
-- a violation of the closed/open invariant, which forbids the rules core casing
-- on the IDENTITY OF AN EFFECT. The test is "is it in the rulebook?" -- Flying is
-- 702.9; Goblin Piker is not. Constructors are ordered by RULE NUMBER, not by
-- arrival, so this type stays diffable against rule 702 itself.
--
-- A keyword is not necessarily a STATIC ability: rule 702.70 spells poisonous out
-- as a TRIGGERED one. What it grants is still a citation and not an effect
-- identity, so Pawl.Engine.Keyword may read a constructor and mint the rule's
-- ability from it.
--
-- Multiplicity is NOT this type's problem: an object can have the same keyword
-- ability twice, which Pawl.Types.ProjectedCharacteristics.keywords carries as a
-- count. This type says only WHICH ability, so a card's printed keywords stay a
-- Set -- see Pawl.Types.Face.keywords.
--
-- This module TIES THE KNOT that Pawl.Types.Filter's keyword parameter opens:
-- Filter has a HasKeyword arm and this type carries a Filter (702.11d, 702.14c,
-- 702.29e) and a Cost (702.29a/702.34a/702.42a) whose components carry one too, so the
-- three would be a module cycle if any were concrete. They are parametric and
-- this one is not, which makes `Filter Keyword` and `Cost Keyword` the only
-- instantiations anywhere.
data Keyword
  = Deathtouch -- 702.2
  | Defender -- 702.3
  | DoubleStrike -- 702.4
  | FirstStrike -- 702.7
  | -- | 702.8a: this card may be played any time you could cast an instant. The
    -- only keyword here about WHEN a card may be cast -- rule 702's other casting
    -- keywords in this pool move a different axis -- and nothing reads it once the
    -- spell is on the stack. Read by Pawl.Engine.Cast.instantSpeed.
    --
    -- Nullary, because rule 702.8a takes no parameter, and CR 702.8b makes
    -- multiple instances redundant -- so its reader takes membership rather than
    -- the per-keyword count the projection carries.
    --
    -- Not a Pawl.Types.CastingPermission: that type's arms name a ZONE a card may
    -- be cast from (CR 601.3), where rule 702.8a names a TIME and no zone at all.
    -- A Pouncing Cheetah in a graveyard is as uncastable as a War Mammoth there.
    Flash
  | Flying -- 702.9
  | Haste -- 702.10
  | -- | 702.11b with Nothing: this permanent can't be the target of spells or
    -- abilities your opponents control. 702.11d with Just: "hexproof from
    -- [quality]" -- this permanent can't be the target of [quality] spells your
    -- opponents control or abilities your opponents control from [quality]
    -- sources.
    --
    -- Shroud's sibling (702.18a) and deliberately NOT the same constructor: the
    -- CONTROLLER AXIS is what separates them, and both arms of this one carry it.
    -- Shroud names no player, so it stops the permanent's own controller as
    -- readily as anyone else; hexproof's "your opponents control" makes the answer
    -- depend on WHO is aiming the spell, which is why
    -- Pawl.Engine.Target.targetable reads CR 109.5's "you" and not only the
    -- candidate. The quality below narrows the same rule a second time, by WHAT is
    -- aiming -- the question protection (702.16b) asks and the one shroud never
    -- does.
    --
    -- The quality RIDES this constructor rather than taking one of its own,
    -- because rule 702.11d's last sentence says "a 'hexproof from [quality]'
    -- ability is a hexproof ability" and rule 702.11e spends three sentences
    -- spelling out what that buys: an effect that removes hexproof removes these
    -- too, an effect that ignores hexproof ignores these too, and an effect that
    -- looks for hexproof finds these too. One constructor makes all three true for
    -- free at every reader instead of restating them at each -- the argument CR
    -- 702.29f makes for typecycling riding Cycling. Humility (CR 613.1f) drops a
    -- "hexproof from black" because it drops KEYWORDS, not because anything
    -- enumerated the pair.
    --
    -- A Filter and not a Color, because rule 702.11d's "[quality]" is any quality:
    -- "hexproof from planeswalkers" (Eradicator Valkyrie) is a card type,
    -- "hexproof from instants" (Elenda, Saint of Dusk) another, and "hexproof from
    -- monocolored" (Sphinx of the Guildpact) is CR 105.2a's exactly-one-colour,
    -- which needs the combinators. Cycling's `Maybe (Filter Keyword)` exactly.
    --
    -- CR 702.11f's "hexproof from [quality A] and from [quality B]" and CR
    -- 702.11g's "hexproof from each [characteristic]" need no arm of their own:
    -- both rules say the card HAS SEVERAL hexproof abilities rather than one
    -- compound one, and Pawl.Types.Face.keywords is a Set, so such a card prints
    -- one entry per quality. CR 702.11h's redundancy is then per key, which is
    -- what its "the same hexproof ability" means.
    Hexproof (Maybe (Filter.Filter Keyword))
  | Indestructible -- 702.12
  | Intimidate -- 702.13
  | -- | 702.14a: "[type]walk", where the type is usually a land type but need not
    -- be. The qualification rides the constructor, as Cycling's Filter does, so
    -- `Landwalk (HasSubtype Swamp)` and `Landwalk (HasSubtype Island)` are
    -- distinct keywords -- which is what CR 702.14d needs, since landwalk
    -- abilities don't cancel one another: Pawl.Engine.Combat.landwalkAllowsGiven
    -- looks up the DEFENDING PLAYER'S lands per landwalk walked, never the
    -- blocker.
    --
    -- A FILTER, not a Subtype, because CR 702.14c names four shapes and only the
    -- first is a bare land type. The third needs a negation and the fourth a
    -- conjunction, neither of which a Subtype can say:
    --
    --   islandwalk         HasSubtype Island
    --   artifact landwalk  HasCardType Artifact
    --   nonbasic landwalk  Not (HasSupertype Basic)
    --   snow swampwalk     And [HasSupertype Snow, HasSubtype Swamp]
    --
    -- The filter carries the QUALIFICATION only, never the land-ness: every clause
    -- of CR 702.14c is about a LAND with or without something, so that conjunct is
    -- the rule's own and landwalkAllowsGiven asks it separately, which also keeps
    -- the CR 205.3d guard. A card cannot forget it.
    --
    -- CR 702.14e makes instances of the same kind redundant, which is why that
    -- reader takes membership rather than a count. "The same kind" is filter
    -- equality here, which is structural: two filters meaning the same thing
    -- written differently count as two kinds, and redundancy still gives the same
    -- answer because the reader is an `any`.
    Landwalk (Filter.Filter Keyword)
  | -- | 702.15b: damage dealt by a source with lifelink causes that source's
    -- controller (or owner) to gain that much life. Nullary, because rule 702.15b
    -- takes no parameter, and CR 702.15f makes multiple instances redundant -- so
    -- its reader takes membership rather than a count. Flying's shape, not
    -- toxic's.
    --
    -- Read ONCE, at deal time, into Pawl.Types.DamageEvent.dealtByLifelink: CR
    -- 702.15c decides lifelink-ness from the source's last known information and
    -- CR 702.15d makes it zone-independent, so this is never asked of a live board
    -- when the life is handed over.
    Lifelink
  | Reach -- 702.17
  | -- | 702.18a: this permanent or player can't be the target of spells or
    -- abilities. The pool's first TARGETING RESTRICTION, read by the CR 115
    -- target-legality gate (Pawl.Engine.Target.targetable), where every
    -- restriction rule 702 states lands.
    --
    -- Nullary, because rule 702.18a takes no parameter, and it asks neither of
    -- the two questions its neighbours do: not hexproof's "your opponents
    -- control" (702.11b/702.11d) about the targeting player, and not the stated
    -- quality of hexproof's variant (702.11d) or of protection (702.16a) about
    -- the source. That it asks neither is exactly why it is a separate keyword
    -- rather than another field on Hexproof above.
    Shroud
  | Trample -- 702.19
  | -- | 702.19c: trample over planeswalkers, a VARIANT of trample that adds a
    -- second tier to CR 702.19b's gate -- once the blockers have lethal and the
    -- attacked planeswalker has damage at least equal to its loyalty, the rest
    -- may go to that planeswalker's controller. CR 702.19e is its other half,
    -- an exception to CR 506.4c.
    --
    -- A SIBLING of Trample rather than a payload on it, because CR 702.19d names
    -- the two side by side ("with trample or trample over planeswalkers"): a
    -- creature can have either without the other, and Pawl.Engine.Damage reads
    -- them as two questions.
    --
    -- Payload-free: rule 702.19c takes no parameter and CR 702.19g makes
    -- multiple instances redundant, so no KeywordFamily constructor is owed.
    TrampleOverPlaneswalkers
  | Vigilance -- 702.20
  | -- | 702.22: banding. Only the two COMBAT-DAMAGE-DIVISION halves are modeled --
    -- CR 702.22j, where a banding blocker moves the choice of how the ATTACKING
    -- creature's damage is divided from the active player to the defending one,
    -- and CR 702.22k, its mirror, where a banding attacker moves the choice of how
    -- a BLOCKING creature's damage is divided the other way. Both are stated as
    -- exceptions, to CR 510.1c and CR 510.1d respectively.
    --
    -- CR 702.22b's blocking half -- a band of creatures attacking as a unit, and
    -- "bands with other" -- is NOT here and has no producer: it needs a band to be
    -- declared as attackers, which is a shape the declare-attackers step does not
    -- have. Benalish Hero, the pool's producer, is a plain banding creature, and
    -- CR 702.22j/k are what its reminder text describes.
    --
    -- Payload-free, unlike Landwalk: plain banding names no quality. The
    -- "[quality] creature with 'bands with other [quality]'" clause both rules
    -- also cover would need one, and is part of the unmodeled half.
    Banding -- 702.22
  | -- | 702.23a: rampage N, a TRIGGERED ability meaning "Whenever this creature
    -- becomes blocked, it gets +N/+N until end of turn for each creature blocking
    -- it beyond the first." Minted by Pawl.Engine.Keyword.rampage and handed to
    -- the ordinary CR 603 machinery, as flanking and bushido are.
    --
    -- Carries its N, unlike flanking below: rule 702.23a takes a parameter, and
    -- the bonus is N TIMES a number read off the board, so the two halves come
    -- from different places -- N from the card, the multiplicand from
    -- Pawl.Types.Quantity.BlockersBeyondFirst.
    --
    -- CR 702.23c makes multiple instances trigger separately, so its reader takes
    -- the per-keyword COUNT the projection carries, exactly as flanking and
    -- bushido do.
    Rampage Natural.Natural
  | -- | 702.25a: "whenever this creature becomes blocked by a creature without
    -- flanking, the blocking creature gets -1/-1 until end of turn". A TRIGGERED
    -- ability, like rule 702.70's poisonous and rule 702.45's bushido, so
    -- Pawl.Engine.Keyword.flanking mints it and the ordinary CR 603 machinery
    -- runs it.
    --
    -- Payload-free: rule 702.25a takes no parameter. Its own "without flanking"
    -- is `Filter.Not (Filter.HasKeyword Flanking)` in the minted condition, which
    -- needs no Pawl.Types.KeywordFamily constructor precisely because there is no
    -- payload to abstract over -- a card asking for "a creature with flanking"
    -- asks for this constructor exactly.
    --
    -- Multiple instances are NOT redundant (CR 702.25b: each triggers
    -- separately), so its reader is the projection's per-keyword count rather
    -- than membership.
    Flanking -- 702.25
  | -- | 702.26a: a static ability that MODIFIES THE RULES OF THE UNTAP STEP.
    -- During each player's untap step, before that player untaps anything, every
    -- phased-in permanent with phasing they control phases out, and every
    -- phased-out permanent that phased out under their control phases in.
    --
    -- Nothing here mints an ability, so Pawl.Engine.Keyword answers this
    -- constructor with [] at every one of its arms. The rule is read directly by
    -- Pawl.Engine.Phasing, from the CR 502.1 turn-based action -- the same shape
    -- as CR 702.145's daybound, whose CR 502.2 check Pawl.Engine.Daytime reads
    -- rather than granting anything.
    --
    -- Payload-free: rule 702.26a takes no parameter, and CR 702.26p makes multiple
    -- instances redundant, so its reader takes membership rather than the
    -- per-keyword count the projection carries.
    --
    -- Phasing is a property of the PERMANENT, not of the phased-out state: a
    -- permanent phased out by an effect (Teferi's Protection) has no phasing
    -- ability and so never phases back in on its own. That is why "is it phased
    -- out" lives on Pawl.Types.GameState and not here.
    Phasing
  | -- | 702.28b: a creature with shadow can't be blocked by creatures without
    -- shadow, and a creature WITHOUT shadow can't be blocked by creatures with
    -- shadow.
    --
    -- The pool's first SYMMETRIC evasion ability, and the reason it is worth a
    -- constructor rather than a rewrite of an existing gate: flying, fear,
    -- intimidate, landwalk and menace all restrict being BLOCKED and read the
    -- keyword off the ATTACKER, where 702.28b's second sentence restricts
    -- BLOCKING too, so the blocker's own shadow disqualifies it from blocking
    -- anything else. Pawl.Engine.Combat.shadowAllows is where the two sentences
    -- collapse into one equality.
    --
    -- Payload-free, because rule 702.28b takes no parameter, and CR 702.28c makes
    -- multiple instances redundant -- so its reader takes membership rather than
    -- the per-keyword count the projection carries.
    Shadow
  | -- | 702.29a: pay the cost and discard this card to draw a card, functioning
    -- only while the card is in a player's hand. The cost rides the constructor,
    -- as Flashback's does, because rule 702.29a states it as part of the keyword
    -- rather than as separate card text; Pawl.Engine.Keyword.handAbilitiesOf mints
    -- the whole ability from this one value.
    --
    -- The hand-only half is NOT a field here: rule 702.29b is explicit that the
    -- ability exists in every zone, so the zone is a question the READER asks
    -- (Pawl.Engine.Activate.abilitiesFor), exactly as Pawl.Engine.Cost.costsFor
    -- asks it of rule 702.34a's flashback cost.
    --
    -- Typecycling (702.29e) is this same ability with a library search in place of
    -- the draw, riding THIS constructor rather than a sibling because CR 702.29f
    -- makes every rule that looks for cycling find it -- one constructor makes that
    -- true for free instead of restating it at each reader. Nothing is plain
    -- cycling; Just is what to search for, and a Filter rather than a Subtype
    -- because rule 702.29e's "[type]" may be any combination of card type, subtype
    -- and supertype.
    Cycling (Cost.Cost Keyword) (Maybe (Filter.Filter Keyword))
  | -- | 702.31b: a creature with horsemanship can't be blocked by creatures
    -- without horsemanship.
    --
    -- Shadow's FIRST SENTENCE and nothing else, which is why it is its own
    -- constructor rather than a flag on that one: rule 702.31b's second sentence
    -- says a creature with horsemanship can block a creature with OR WITHOUT it,
    -- so horsemanship keeps the asymmetry flying, fear, intimidate and skulk have
    -- and shadow gives up. Pawl.Engine.Combat.horsemanshipAllows reads it off the
    -- ATTACKER first for exactly that reason.
    --
    -- Payload-free, because rule 702.31b takes no parameter, and CR 702.31c makes
    -- multiple instances redundant -- so its reader takes membership rather than
    -- the per-keyword count the projection carries.
    Horsemanship
  | -- | 702.33a: "You may pay an additional [cost] as you cast this spell", and CR
    -- 702.33d's designation for the spell whose controller declares they will --
    -- that spell has been "kicked".
    --
    -- The cost rides the constructor, as Flashback's and Entwine's do, because
    -- rule 702.33a states it as part of the keyword. It is NOT a
    -- Face.additionalCosts entry: that list is unconditioned, so a kicker cost
    -- placed there would be paid by every cast, and declining it is precisely the
    -- player's choice under CR 601.2b.
    --
    -- The PAYOFF is not a field either. Rule 702.33e makes it a separate ability
    -- of the card ("objects with kicker ... have additional abilities that specify
    -- what happens if they were kicked"), so it is printed text like any other --
    -- a clause condition on Quantity.WasKicked, which reads the designation
    -- Pawl.Engine.Cast stamped.
    --
    -- CR 702.33c's multikicker is not this constructor with a variant, the shape
    -- Morph took for megamorph: "any number of times" turns the announcement into
    -- a count rather than a yes-or-no, so nothing here is reusable past the cost
    -- (#1234). CR 702.33b's second kicker cost, CR 702.33f's "kicked with its [A]
    -- kicker" and CR 702.33h's sticker kicker are unrepresented for the same
    -- reason -- one designation cannot say WHICH cost was paid (#1235).
    Kicker (Cost.Cost Keyword)
  | -- | 702.34a: this card may be cast from its owner's graveyard for the given
    -- cost, and is exiled instead of going anywhere else as it leaves the stack.
    --
    -- The cost rides the constructor, as Toxic's N does, because rule 702.34a
    -- states it as part of the keyword. It is deliberately NOT a
    -- Face.alternativeCosts entry: that list is unconditioned, so a flashback cost
    -- placed there would also be payable from the HAND. Pawl.Engine.Keyword turns
    -- this one value into all three of the rule's consequences -- the cost (read
    -- by Pawl.Engine.Cost.costsFor only in the graveyard), the permission and the
    -- exile replacement.
    Flashback (Cost.Cost Keyword)
  | Fear -- 702.36
  | -- | 702.37a: "You may cast this card as a 2/2 face-down creature with no
    -- text, no name, no subtypes, and no mana cost by paying {3} rather than
    -- paying its mana cost", plus CR 702.37e's special action -- any time you
    -- have priority you may turn a face-down permanent you control with a morph
    -- ability face up by paying this cost.
    --
    -- The cost rides the constructor, as Flashback's and Entwine's do, because
    -- rule 702.37a states it as part of the keyword. It is the cost of the
    -- SPECIAL ACTION and never of the cast: the {3} a morph cast pays is written
    -- into rule 702.37a itself and so is minted by
    -- Pawl.Engine.Cost.faceDownCost rather than read from here. That is the
    -- whole reason this is not a Face.alternativeCosts entry -- an alternative
    -- cost there would be payable to cast the card FACE UP, which is the one
    -- thing a morph ability does not offer.
    --
    -- CR 702.37b's megamorph is THIS constructor with a variant rather than a
    -- sibling beside it, because the rule says so twice over: "Megamorph is a
    -- variant of the morph ability" and "A megamorph cost is a morph cost". See
    -- Pawl.Types.MorphVariant for what the variant adds and for what a sibling
    -- constructor would have cost.
    Morph (Cost.Cost Keyword) MorphVariant.MorphVariant
  | -- | 702.39a: whenever this creature attacks, you may choose to have target
    -- creature defending player controls block this creature this combat if able;
    -- if you do, untap that creature. Rule 702 states it as a triggered ability,
    -- and the first whose payload creates a CR 509.1c BLOCKING
    -- REQUIREMENT -- Pawl.Engine.Keyword.provoke mints it, slot and all.
    --
    -- Payload-free, so no Pawl.Types.KeywordFamily constructor is owed. Its reader
    -- takes the per-keyword COUNT rather than membership: CR 702.39b says each
    -- instance triggers separately, so a creature with provoke twice puts two
    -- abilities on the stack and each chooses its own target.
    --
    -- "DEFENDING PLAYER CONTROLS" rides the ability's target slot as mentor's
    -- power comparison does, and for the same reason: it is a fact about the pair.
    -- Filter.ControlledByDefendingPlayer is the atom, and NOT ControlledBy
    -- Opponent, which CR 506.2a makes too wide on a board with three seats.
    Provoke
  | -- | 702.42a: you may choose all modes of this modal spell (rule 700.2) instead
    -- of the number specified, paying an additional cost if you do.
    --
    -- The cost rides the constructor, as Flashback's and Cycling's do. It is NOT a
    -- Face.additionalCosts entry: that list is unconditioned, so an entwine cost
    -- placed there would be paid by every cast, and declining it is precisely the
    -- player's choice under CR 601.2b.
    --
    -- The MODE-WIDENING half is not a field either. Rule 702.42a fixes it
    -- completely -- all modes, never some other number -- so Pawl.Engine.Cast reads
    -- the payload's own Modal.modeCount rather than a number restated here, and
    -- Pawl.Types.ModeSelection stays what the card PRINTS.
    Entwine (Cost.Cost Keyword)
  | -- | 702.43a: modular N, which is TWO abilities -- "this permanent enters with
    -- N +1/+1 counters on it" and "when this permanent is put into a graveyard
    -- from the battlefield, you may put a +1/+1 counter on target artifact
    -- creature for each +1/+1 counter on this permanent". Vanishing's split
    -- across Pawl.Engine.Keyword's two mints, one ability shorter: the first is a
    -- CR 614.1c entry replacement (mintedReplacementsFor) and the second an
    -- ordinary triggered ability.
    --
    -- N rides the constructor, as Vanishing's does, and CR 702.43b says each
    -- instance works separately -- so its readers take the per-keyword COUNT, and
    -- a permanent with modular twice enters with both lots of counters and dies
    -- with two triggers.
    --
    -- The DEATH half reads no N at all: the rule counts the +1/+1 counters
    -- actually on the permanent, so two instances of `Modular 1` each move the
    -- whole pile rather than one counter apiece.
    --
    -- NOT a creature-only keyword: rule 702.43a says "permanent", and Power Depot
    -- (an artifact land) prints it, so nothing here or in the mint asks whether
    -- the bearer is a creature. Rule 702.43b's "creature" is the narrower word,
    -- and narrowing the multiplicity clause alone would be a distinction without
    -- a reader.
    --
    -- A Natural and not a Quantity: CR 702.44c's "Modular--Sunburst" (Arcbound
    -- Wanderer) is the one printing whose N is another keyword's count, and
    -- sunburst itself has no representation yet (#877).
    Modular Natural.Natural
  | -- | 702.45a: whenever this creature blocks or becomes blocked, it gets +N/+N
    -- until end of turn. N rides the constructor, as Poisonous' does, and for the
    -- same reason: CR 702.45b says each instance triggers separately.
    --
    -- ONE constructor for the rule's TWO trigger events (CR 509.3a and CR
    -- 509.3c), because rule 702.45a prints one ability. The split into two
    -- TriggeredAbility values is Pawl.Engine.Keyword.bushido's problem.
    Bushido Natural.Natural
  | -- | 702.46a: soulshift N, a TRIGGERED ability meaning "When this permanent is
    -- put into a graveyard from the battlefield, you may return target Spirit card
    -- with mana value N or less from your graveyard to your hand." Minted by
    -- Pawl.Engine.Keyword.soulshift on afterlife's terms -- the same CR 700.4 dies
    -- event, so the condition is TriggerCondition.SelfDies.
    --
    -- N rides the constructor, as Afterlife's does: it is the mana value bound the
    -- rule's ability filters on, so `Soulshift 3` and `Soulshift 4` are distinct
    -- keywords. CR 702.46b makes multiple instances trigger separately, so its
    -- reader takes the per-keyword COUNT rather than membership -- Forked-Branch
    -- Garami prints "soulshift 4, soulshift 4" and returns two cards.
    Soulshift Natural.Natural
  | -- | 702.61a: split second. "As long as this spell is on the stack, players
    -- can't cast spells or activate abilities that aren't mana abilities."
    --
    -- A static ability of an object ON THE STACK, which is what makes it unlike
    -- every other keyword here: it is not minted into an ability object and it
    -- says nothing about its own spell. It is a RULES-MODIFYING continuous
    -- effect (CR 611.1's third clause) that other players' gates ask about, so
    -- Pawl.Engine.SplitSecond is the reader and Pawl.Engine.Cast and
    -- Pawl.Engine.Activate are the two askers.
    --
    -- Nullary, and CR 702.61c makes multiple instances redundant -- so its
    -- reader takes membership rather than the per-keyword count. No
    -- Pawl.Types.KeywordFamily constructor is owed: that type holds only the
    -- payload-carrying keywords, since a nullary keyword's family would be the
    -- keyword.
    SplitSecond
  | -- | 702.63a: vanishing N, which is THREE abilities -- "this permanent enters
    -- with N time counters on it", "at the beginning of your upkeep, if this
    -- permanent has a time counter on it, remove a time counter from it", and
    -- "when the last time counter is removed from this permanent, sacrifice it".
    -- Pawl.Engine.Keyword mints all three: the first as a CR 614.1c entry
    -- replacement (mintedReplacementsFor, riot's position), the other two as
    -- ordinary triggered abilities.
    --
    -- N rides the constructor, as Bushido's does, and CR 702.63c says each
    -- instance works separately -- so its readers take the per-keyword COUNT,
    -- and a permanent with vanishing twice enters with both lots of counters
    -- and counts them down twice.
    --
    -- NOT a permanent-type-specific keyword: rule 702.63a says "permanent", and
    -- Reality Acid (an Aura) and Four Knocks (an enchantment) print it, so
    -- nothing here or in the mint asks whether the bearer is a creature.
    --
    -- Not implemented: CR 702.63b's vanishing WITHOUT a number, which is the
    -- last two abilities and no entry rewrite -- the payload would have to be a
    -- Maybe (#1186).
    Vanishing Natural.Natural
  | -- | 702.70a: whenever this creature deals combat damage to a player, that
    -- player gets N poison counters. N rides the constructor, as Toxic's does.
    -- Unlike toxic, the N values are NOT summed: CR 702.70b says each instance
    -- triggers separately, so `Poisonous 1` twice is two abilities and two
    -- triggers -- which is what Pawl.Engine.Keyword.triggeredAbilitiesOf builds
    -- from the projection's per-keyword count.
    Poisonous Natural.Natural
  | -- | 702.73a: "This object is every creature type." A
    -- CHARACTERISTIC-DEFINING ability (CR 604.3), so it defines a
    -- characteristic rather than minting anything: it lands in CR 613.1d's
    -- layer 4 through Pawl.Engine.Projection.applySubtypeDefining, and off the
    -- battlefield through viewOfCard, exactly as CR 702.114a's devoid lands in
    -- layer 5.
    --
    -- Nullary, because rule 702.73a takes no parameter, so no
    -- Pawl.Types.KeywordFamily constructor is owed. Its reader takes
    -- MEMBERSHIP rather than a count: a second instance defines the same set of
    -- creature types, so it is the identity -- redundant by arithmetic, where
    -- CR 702.80d's wither says so outright.
    Changeling
  | -- | 702.77a: reinforce N-[cost], an ACTIVATED ability that functions only
    -- while the card is in a player's hand, meaning "[cost], Discard this card:
    -- Put N +1/+1 counters on target creature." Cycling's zone and cycling's
    -- discard-in-the-cost shape (702.29a); what is new is a target, so
    -- Pawl.Engine.Keyword.reinforce is the first hand ability that has one.
    --
    -- BOTH halves ride the constructor, and neither is redundant: the cost is
    -- what is paid, as Cycling's and Flashback's are, and N is how many counters
    -- the minted ability puts on -- so `Reinforce 1 c` and `Reinforce 2 c` are
    -- distinct keywords. Rule 702.77 states no redundancy clause and no card
    -- prints two, so its reader takes MEMBERSHIP, which is the Set
    -- Pawl.Engine.Keyword.handAbilitiesOf already takes.
    --
    -- Rule 702.77b's other half -- the ability keeps existing in every other
    -- zone, so an object with reinforce counts as having an activated ability --
    -- is not modelled (#1207).
    Reinforce Natural.Natural (Cost.Cost Keyword)
  | -- | 702.79a: persist. "When this permanent is put into a graveyard from the
    -- battlefield, if it had no -1/-1 counters on it, return it to the
    -- battlefield under its owner's control with a -1/-1 counter on it" -- which
    -- CR 700.4 is what makes that a dies trigger. A triggered ability rule 702
    -- states in full, so
    -- Pawl.Engine.Keyword.persist mints it and no card writes the sentence.
    --
    -- Undying's (702.93a) exact mirror, down to the "if" clause, and a separate
    -- constructor rather than one keyword carrying a CounterKind: the two are two
    -- rules with two names, and a card asking for "a creature with persist" is
    -- asking about rule 702.79 rather than about a counter kind.
    --
    -- Nullary, because rule 702.79a takes no parameter, and its reader takes the
    -- per-keyword COUNT: rule 702.79 states no redundancy clause, so CR 603.2's
    -- general reading makes two instances two abilities -- two returns, the second
    -- of which finds the permanent already back and does nothing.
    Persist
  | -- | 702.80a: damage this source deals to a creature isn't marked on it;
    -- instead its controller puts that many -1/-1 counters on that creature.
    -- Nullary, because CR 702.80d makes multiple instances redundant -- so unlike
    -- poisonous and battle cry, no reader here takes a count.
    --
    -- Infect's (702.90) CREATURE half and nothing more: CR 120.3d names both
    -- keywords together, while CR 120.3a's life-loss exception names infect
    -- alone, so wither damage to a player is ordinary life loss.
    Wither
  | -- | 702.83a: whenever a creature you control attacks alone, that creature
    -- gets +1/+1 until end of turn. Rule 702 states it as a triggered ability,
    -- and the first whose ability watches a permanent that is
    -- not its bearer: "a creature you control" is every creature its controller
    -- has, so an untapped Aven Squire triggers off another creature's attack.
    -- Pawl.Engine.Keyword.exalted mints it, and
    -- TriggerCondition.CreatureAttacksAlone is the condition that says it.
    --
    -- Nullary, because rule 702.83a takes no parameter -- and its reader takes the
    -- per-keyword COUNT rather than membership: rule 702.83 prints no
    -- "multiple instances are redundant" clause, unlike the static keywords that
    -- do (CR 702.28c's shadow), so two instances are two triggered abilities and
    -- +2/+2.
    Exalted
  | -- | 702.86a: whenever this creature attacks, defending player sacrifices N
    -- permanents. N rides the constructor, as Poisonous' does, and for the same
    -- reason: CR 702.86b says each instance triggers separately, so
    -- `Annihilator 1` twice is two abilities and two sacrifices rather than one
    -- ability for 2 -- which is what Pawl.Engine.Keyword.triggeredAbilitiesOf
    -- builds from the projection's per-keyword count.
    --
    -- Rule 702 states it as a triggered ability, and it is minted the way
    -- poisonous (702.70a) and battle cry (702.91a) are. What it adds to those
    -- two is the PLAYER: rule 702.86a names the defending
    -- player, whom CR 508.5 reads off what the creature is attacking -- so
    -- Pawl.Engine.Combat.declareAttackers computes it as the attack is declared
    -- and GameEvent.AttackerDeclared carries it, and the minted ability reads it
    -- back through the reserved Pawl.Engine.Binding.triggerPlayer slot.
    Annihilator Natural.Natural
  | Infect -- 702.90
  | -- | 702.91a: whenever this creature attacks, each other attacking creature gets
    -- +1/+0 until end of turn. Rule 702 states it as a triggered ability, so
    -- Pawl.Engine.Keyword MINTS it rather than merely consulting it, as it does
    -- poisonous (702.70a) and annihilator (702.86a). Nullary, because rule
    -- 702.91a takes no parameter -- and unlike flying's or lifelink's nullary
    -- siblings, its reader takes the per-keyword count rather than membership,
    -- since CR 702.91b gives it the multiplicity CR 702.70b gives poisonous.
    BattleCry
  | -- | 702.93a: undying, rule 702.79a's sentence in the other counter kind --
    -- "if it had no +1/+1 counters on it, return it to the battlefield under its
    -- owner's control with a +1/+1 counter on it". Persist's mirror, and
    -- minted by Pawl.Engine.Keyword.undying for that constructor's reasons,
    -- including why the two are not one.
    --
    -- Nullary and count-read, for persist's reasons.
    Undying
  | -- | 702.98a: unleash. TWO static abilities, as rule 702.98a bundles them:
    -- "You may have this permanent enter with an additional +1/+1 counter on it"
    -- and "This permanent can't block as long as it has a +1/+1 counter on it."
    --
    -- The first is riot's first half exactly, so it is a CR 614.1c as-enters
    -- replacement minted by Pawl.Engine.Keyword.mintedReplacementsOf. The second
    -- is a CR 509.1b combat restriction, minted by
    -- Pawl.Engine.Keyword.mintedCombatRestrictionsOf -- the first keyword to mint
    -- one, where defender (CR 702.3b) is read as a keyword inside
    -- Pawl.Engine.Combat because its restriction has no gate to carry.
    --
    -- The second half is NOT conditional on the first: the rule says "a +1/+1
    -- counter", not "that counter", so a counter arriving any other way shuts
    -- blocking off just as well, and removing the counter turns it back on.
    --
    -- Nullary, because rule 702.98a takes no parameter, and its two readers
    -- differ. Two instances are two of EACH static ability -- rule 702.98a has no
    -- "each instance" sentence of riot's (CR 702.136b), but two copies of an
    -- ability is what two instances of a keyword representing them are. So the
    -- entry half takes the per-keyword COUNT, offering two counters, while the
    -- restriction half is MEMBERSHIP: two copies of "can't block" forbid the same
    -- block once.
    Unleash
  | -- | 702.100a: whenever a creature you control enters, if that creature's power
    -- and/or toughness is greater than this creature's, put a +1/+1 counter on
    -- this creature. Minted by Pawl.Engine.Keyword.evolve like the triggered
    -- keywords around it.
    --
    -- The first minted ability whose intervening "if" (CR 603.4) is about the
    -- EVENT's object rather than the bearer -- rule 702.112a's renown asks about
    -- the source, this one about the entrant -- and the first Condition in the
    -- pool that needs a disjunction, rule 702.100a's "and/or" comparing two
    -- different characteristics.
    --
    -- Nullary: rule 702.100a takes no parameter, so no KeywordFamily is owed. Its
    -- reader takes the per-keyword count rather than membership, CR 702.100d
    -- giving it the multiplicity CR 702.108b gives prowess.
    Evolve
  | -- | 702.105a: whenever this creature attacks the player with the most life or
    -- tied for most life, put a +1/+1 counter on it. Rule 702 states it as a
    -- triggered ability, minted by Pawl.Engine.Keyword.dethrone like the triggered
    -- keywords around it.
    --
    -- What it adds to those is a condition that reads the LIFE TOTALS of every
    -- player still in the game as the declaration happens
    -- (TriggerCondition.SelfAttacksPlayerWithMostLife), where every other minted
    -- attack trigger asks only about the declaration itself.
    --
    -- Nullary: rule 702.105a takes no parameter, so no KeywordFamily is owed. Its
    -- reader takes the per-keyword count rather than membership, CR 702.105b
    -- giving it the multiplicity CR 702.108b gives prowess.
    Dethrone
  | -- | 702.107a: outlast [cost], an ACTIVATED ability meaning "[Cost], {T}: Put
    -- a +1/+1 counter on this creature. Activate only as a sorcery."
    -- Pawl.Engine.Keyword.outlast mints the whole ability from this one value, as
    -- rule 702.122a's crew is minted from Crew -- both function on the
    -- battlefield, where cycling's functions in a hand.
    --
    -- The cost rides the constructor, as Flashback's and Cycling's do, because
    -- rule 702.107a states it as part of the keyword. What the card supplies is
    -- only that cost: the tap symbol, the counter and the sorcery-speed clause
    -- are the rule's own, so they are written into the minted ability rather
    -- than into card data. That is also why this is a Cost and not Crew's bare
    -- Natural -- rule 702.107a's "[cost]" is a cost the card names.
    --
    -- Rule 702.107 prints no multiplicity clause, and CR 702.107a states a whole
    -- self-contained ability, so its reader takes the per-keyword COUNT the
    -- projection carries: outlast twice is two activatable abilities, crew's
    -- reading rather than shadow's redundancy.
    Outlast (Cost.Cost Keyword)
  | -- | 702.108a: whenever you cast a noncreature spell, this creature gets +1/+1
    -- until end of turn. Rule 702 states it as a triggered ability, minted the
    -- way poisonous (702.70a), annihilator (702.86a) and battle cry (702.91a)
    -- are. What it adds to those three is the WATCHED EVENT: the first minted
    -- trigger on something other than its
    -- own bearer's combat, so the minted condition is CR 601.2i's
    -- TriggerCondition.SpellCast rather than a self-scoped one.
    --
    -- Nullary, because rule 702.108a takes no parameter, and its reader takes
    -- the per-keyword count rather than membership: CR 702.108b gives it the
    -- multiplicity CR 702.91b gives battle cry.
    Prowess
  | -- | 702.111b: a creature with menace can't be blocked except by two or more
    -- creatures. Nullary like fear (702.36) and unlike landwalk -- the number two
    -- is written into the rule.
    --
    -- The blocking side's SET-SHAPED combat restriction, whose attacking
    -- counterpart is Bonded Construct's "can't attack alone".
    -- Every other evasion ability here asks about one (blocker, attacker) pair or
    -- less; menace asks how MANY creatures are blocking, which no pairwise
    -- predicate can answer, so it is read by
    -- Pawl.Engine.Combat.menaceAllowsGiven -- a whole-declaration function --
    -- rather than beside the other three in pairAllowedGiven.
    Menace
  | -- | 702.112a: renown N, a TRIGGERED ability meaning "When this creature deals
    -- combat damage to a player, if it isn't renowned, put N +1/+1 counters on it
    -- and it becomes renowned." Minted by Pawl.Engine.Keyword.renown and handed to
    -- the ordinary CR 603 machinery, as poisonous (702.70a) and training (702.149a)
    -- are. The first minted ability with an intervening "if" -- rule 702.112a
    -- prints one, and CR 603.4 puts it on TriggeredAbility.intervening.
    --
    -- N rides the constructor, as Poisonous' does, and for the same reason: it is
    -- how many counters the rule's ability places, so `Renown 1` and `Renown 2` are
    -- distinct keywords. CR 702.112c makes multiple instances trigger separately,
    -- so its reader takes the per-keyword COUNT the projection carries rather than
    -- membership.
    --
    -- RENOWNED is not here, and rule 702.112b is why: it is a designation on the
    -- permanent rather than an ability, and "neither an ability nor part of the
    -- permanent's copiable values". It rides Pawl.Types.Object.renowned, beside the
    -- Ring-bearer designation and a battle's protector.
    Renown Natural.Natural
  | Devoid -- 702.114
  | -- | 702.118b: a creature with skulk can't be blocked by creatures with greater
    -- power.
    --
    -- Asymmetric, the shape flying, fear, intimidate, landwalk and menace share
    -- and shadow does not: 702.118b restricts being BLOCKED and says nothing
    -- about blocking. What it adds to them is the EXCEPTION -- a comparison
    -- between the two creatures rather than a property of the blocker alone.
    -- Intimidate (702.13b) is the other gate written that way, over colours
    -- instead of power, so neither can be expressed as the other. Read by
    -- Pawl.Engine.Combat.skulkAllowsGiven.
    --
    -- Both powers come off the PROJECTION, never the printed box: CR 509.1b is
    -- checked as blockers are declared, so what counts is the power CR 613 gives
    -- each creature at that moment.
    --
    -- Payload-free, because rule 702.118b takes no parameter, and CR 702.118c
    -- makes multiple instances redundant -- so its reader takes membership rather
    -- than the per-keyword count the projection carries.
    Skulk
  | -- | 702.121a: melee, a TRIGGERED ability meaning "Whenever this creature
    -- attacks, it gets +1/+1 until end of turn for each opponent you attacked
    -- with a creature this combat." Minted by Pawl.Engine.Keyword.melee and
    -- handed to the ordinary CR 603 machinery, as battle cry and prowess are.
    --
    -- Payload-free: rule 702.121a takes no parameter. What varies is the BONUS,
    -- which the rule computes from the combat record rather than from anything
    -- the card prints -- Pawl.Types.Quantity.OpponentsAttacked is that reading,
    -- and it is why this keyword needs no N where poisonous and afflict do.
    --
    -- CR 702.121b makes multiple instances trigger separately, so its reader
    -- takes the per-keyword COUNT the projection carries, exactly as flanking and
    -- bushido do, and not membership as skulk above does.
    Melee
  | -- | 702.122a: crew N, an ACTIVATED ability meaning "Tap any number of other
    -- untapped creatures you control with total power N or greater: This
    -- permanent becomes an artifact creature until end of turn."
    -- Pawl.Engine.Keyword.crew mints the whole ability from this constructor,
    -- the way rule 702.29a's cycling is minted from Cycling -- the difference
    -- being the ZONE, since rule 702.122a's ability functions on the
    -- battlefield rather than in a hand.
    --
    -- N rides the constructor, Toxic's shape: `Crew 1` and `Crew 6` are
    -- distinct keywords. Unlike toxic the Ns are never summed -- CR 702.122a
    -- states one self-contained ability, so two crew abilities are two
    -- activatable abilities with two separate thresholds, which is the
    -- per-instance reading Poisonous takes.
    --
    -- A Natural and not a Cost, unlike Cycling and Flashback: rule 702.122a's
    -- cost is not a Cost the card names, it is a SHAPE the rule states, and the
    -- only thing the card supplies is the threshold.
    Crew Natural.Natural
  | -- | 702.123a: fabricate N, a TRIGGERED ability meaning "When this permanent
    -- enters, you may put N +1/+1 counters on it. If you don't, create N 1/1
    -- colorless Servo artifact creature tokens." Minted by
    -- Pawl.Engine.Keyword.fabricate, as rule 702.135a's afterlife is.
    --
    -- Rule 702.123a prints CR 118.12a's rewriting already done, which is why the
    -- minted clause is an ordinary Pawl.Types.UnlessPaid over
    -- Pawl.Types.CostComponent.PutPlusOneCountersOnThis and needs no branching
    -- opcode: the counters are the cost, the tokens the "if you don't" branch.
    --
    -- N rides the constructor, as Afterlife's does: it is both how many counters
    -- and how many tokens, so `Fabricate 1` and `Fabricate 2` are distinct
    -- keywords. CR 702.123b makes multiple instances trigger separately, so its
    -- reader takes the per-keyword COUNT rather than membership.
    Fabricate Natural.Natural
  | -- | 702.127a: an ability found on some split cards, and THREE static abilities
    -- in one word -- "you may cast this half of this split card from your
    -- graveyard", "this half of this split card can't be cast from any zone other
    -- than a graveyard", and "if this spell was cast from a graveyard, exile it
    -- instead of putting it anywhere else any time it would leave the stack".
    --
    -- Flashback's near-twin (702.34a), and deliberately NOT that constructor with
    -- an absent cost: flashback carries an alternative cost and this pays the
    -- printed one, and flashback adds no prohibition where rule 702.127a's second
    -- clause forbids the hand outright. Pawl.Engine.Keyword mints the first and
    -- third from this constructor exactly as it does flashback's, and
    -- Pawl.Engine.Cast reads the second at its Zone.Hand arm.
    --
    -- Payload-free: rule 702.127a takes no parameter -- what it costs is the half's
    -- own mana cost.
    --
    -- "This HALF" is the rule's own scoping and needs nothing here: pawl's
    -- keywords live on a Face, and Pawl.Engine.Cast already reasons about one
    -- proposed half at a time, so an aftermath half restricts itself and leaves its
    -- sibling alone.
    Aftermath
  | -- | 702.130a: whenever this creature becomes blocked, defending player loses
    -- N life. N rides the constructor, as Poisonous' does, and for the same
    -- reason: CR 702.130b says each instance triggers separately.
    --
    -- Rule 702 states it as a triggered ability. Its event is CR
    -- 509.3c's, which Bushido above already watches, and its PLAYER is CR 508.5's,
    -- which Annihilator above already reads -- so Pawl.Engine.Keyword.afflict is
    -- the two put together and adds nothing of its own.
    Afflict Natural.Natural
  | -- | 702.133a: two static abilities -- "you may cast this card from your
    -- graveyard if the resulting spell is an instant or sorcery spell by
    -- discarding a card as an additional cost to cast it", and "if this spell was
    -- cast using its jump-start ability, exile this card instead of putting it
    -- anywhere else any time it would leave the stack".
    --
    -- Flashback's other near-twin, beside Aftermath, and a third constructor
    -- rather than either of those two because the COST is a third shape: rule
    -- 702.34a replaces the mana cost, rule 702.127a replaces nothing, and this one
    -- ADDS to it (CR 601.2b/601.2f-h, additional rather than alternative). Its
    -- instant-or-sorcery gate is flashback's word for word and aftermath has
    -- none; its exile is word for word both of theirs, so
    -- Pawl.Engine.Keyword.castFromGraveyardExile is shared rather than copied.
    --
    -- Payload-free: rule 702.133a names the discard itself, so the card supplies
    -- nothing -- which is why no Pawl.Types.KeywordFamily constructor is owed
    -- here, where flashback's cost owes one.
    JumpStart
  | -- | 702.134a: whenever this creature attacks, put a +1/+1 counter on target
    -- attacking creature with power less than this creature's power. Rule 702
    -- states it as a triggered ability, and the first whose ability
    -- TARGETS -- Pawl.Engine.Keyword.mentor mints it, slot and all.
    --
    -- Payload-free, because rule 702.134a takes no parameter, and its reader takes
    -- the per-keyword COUNT rather than membership: CR 702.134b says each instance
    -- triggers separately, so a creature with mentor twice puts two abilities on
    -- the stack and each chooses its own target.
    --
    -- The power comparison rides the ability's TARGET SLOT rather than anything
    -- here: it is a fact about the pair, so it is Filter.PowerLessThanSource in the
    -- minted spec, the way rule 702.118b's comparison is written into
    -- Pawl.Engine.Combat rather than into Skulk.
    --
    -- CR 702.134c -- "an ability that triggers whenever a creature mentors another
    -- creature" -- is a trigger condition no card in pawl's pool prints, so nothing
    -- watches for it (#1159).
    Mentor
  | -- | 702.135a: afterlife N, a TRIGGERED ability meaning "When this permanent is
    -- put into a graveyard from the battlefield, create N 1/1 white and black
    -- Spirit creature tokens with flying." Minted by
    -- Pawl.Engine.Keyword.afterlife on undying's and persist's terms -- rule
    -- 702.135a states the same dies event those two do (CR 700.4), so the
    -- condition is TriggerCondition.SelfDies.
    --
    -- N rides the constructor, as Poisonous' does: it is how many tokens the
    -- rule's ability creates, so `Afterlife 1` and `Afterlife 2` are distinct
    -- keywords. CR 702.135b makes multiple instances trigger separately, so its
    -- reader takes the per-keyword COUNT the projection carries rather than
    -- membership.
    Afterlife Natural.Natural
  | -- | 702.136a: riot. A STATIC ability meaning "You may have this permanent
    -- enter with an additional +1/+1 counter on it. If you don't, it gains
    -- haste." -- so what it creates is a CR 614.1c as-enters replacement effect,
    -- and Pawl.Engine.Keyword.mintedReplacementsOf mints one from this
    -- constructor the way rule 702.70a's ability is minted from Poisonous.
    --
    -- Nullary, because rule 702.136a takes no parameter, and its reader takes the
    -- per-keyword COUNT rather than membership: CR 702.136b says each instance
    -- works separately, so a creature with riot twice is offered the choice
    -- twice and may take a counter for one and haste for the other.
    --
    -- The two halves are deliberately NOT split across two constructors or
    -- folded into an existing rewrite. The counter half goes through CR 122.6's
    -- funnel so CR 614.16 sees it (Doubling Season doubles riot's counter), and
    -- the haste half is a CR 611.2 continuous effect with no duration -- neither
    -- is a copiable value, which is what rules out reusing
    -- Pawl.Types.EntryOption's keyword set (CR 707.2).
    Riot
  | -- | 702.145b: daybound, the front-face half of the pair rule 702.145 states.
    -- Three static abilities in one keyword, exactly as the rule bundles them:
    -- "if it is night and this permanent is represented by a double-faced card,
    -- it enters transformed", "as it becomes night, if this permanent is front
    -- face up, transform it", and "this permanent can't transform except due to
    -- its daybound ability". Pawl.Engine.Daytime is where all three are read, so
    -- nothing mints an ability object from this -- StartYourEngines' shape rather
    -- than Cycling's.
    --
    -- Nullary, because rule 702.145b takes no parameter, and every reader takes
    -- MEMBERSHIP rather than a count: rule 702.145c asks whether a player
    -- controls "a permanent that is front face up with daybound", so a second
    -- instance turns nothing over twice.
    --
    -- The DESIGNATION the rule reads is not here either. CR 731.1 puts day and
    -- night on the GAME, so it is GameState.daytime, the way CR 725.1's monarch
    -- is -- this keyword is only what makes a permanent care.
    Daybound
  | -- | 702.145e: nightbound, daybound's back-face mirror, and two static
    -- abilities rather than three: "as it becomes day, if this permanent is back
    -- face up, transform it" and "this permanent can't transform except due to
    -- its nightbound ability". The missing third is daybound's enters-transformed
    -- clause, which rule 702.145e does not state -- a card is cast from its front
    -- face, so the back face has no entry of its own to rewrite.
    --
    -- Nullary and membership-read, for Daybound's reasons. A separate constructor
    -- and not `Daybound`-with-a-side, because the two abilities differ in which
    -- designation they watch and in which face they look for, and CR 702.145d and
    -- CR 702.145g differ further still: daybound's makes it DAY unconditionally,
    -- where nightbound's makes it night only if no permanent with daybound is on
    -- the battlefield.
    Nightbound
  | -- | 702.147a: decayed. A static ability and a triggered ability: "This
    -- creature can't block" and "When this creature attacks, sacrifice it at end
    -- of combat."
    --
    -- The first is a CR 509.1b combat restriction, unleash's carrier
    -- (Pawl.Engine.Keyword.mintedCombatRestrictionsFor) with no gate and no
    -- counter clause -- rule 702.147a states it flat. The second is
    -- Pawl.Engine.Keyword.decayed, and is the first minted ability to ARM a CR
    -- 603.7 delayed one: what it arms is Pawl.Engine.Keyword.mintedDelayedAbilities,
    -- rule 702's own declaration, where every other Effect.ArmDelayedTrigger names
    -- a Face.delayedAbilities entry the card printed.
    --
    -- Nullary, because rule 702.147a takes no parameter. Rule 702.147 states no
    -- "each instance" sentence, so the restriction half is MEMBERSHIP -- a second
    -- "can't block" forbids the same block once -- while the trigger half takes
    -- the per-keyword count for CR 603.2's general reason, two copies of an
    -- ability being two abilities.
    Decayed
  | -- | 702.149a: whenever this creature and at least one other creature with power
    -- greater than this creature's power attack, put a +1/+1 counter on this
    -- creature. Rule 702 states it as a triggered ability;
    -- Pawl.Engine.Keyword.training mints it.
    --
    -- Payload-free, because rule 702.149a takes no parameter, and its reader takes
    -- the per-keyword COUNT rather than membership: CR 702.149b says each instance
    -- triggers separately, so a creature with training twice gets two counters.
    --
    -- Mentor's comparison with the sides swapped, and it rides the TRIGGER
    -- CONDITION rather than a target slot, because rule 702.149a targets nothing
    -- and asks about the declaration instead -- TriggerCondition.SelfAttacksWithAnother
    -- carrying Filter.PowerGreaterThanSource.
    --
    -- CR 702.149c -- "whenever this creature trains", an ability that triggers on a
    -- resolving training ability putting counters on -- is a trigger condition
    -- nothing here watches for (#1163).
    Training
  | -- | 702.164a: toxic N. N rides the constructor, so `Toxic 1` and `Toxic 2` are
    -- distinct keywords, and CR 702.164b's total toxic value is the sum over every
    -- toxic ability the creature has (Pawl.Engine.Projection.totalToxic) --
    -- including two with the same N, which the projection counts separately.
    Toxic Natural.Natural
  | -- | 702.179a: "start your engines!". A STATIC ability whose whole content is
    -- a state-based action -- CR 704.5z gives a player with no speed who controls
    -- a permanent with this keyword a speed of 1 -- so nothing mints an ability
    -- from it and Pawl.Engine.Sba reads it off the projection directly, the way
    -- flying is read where it matters rather than turned into an object.
    --
    -- Nullary, because rule 702.179a takes no parameter, and its reader takes
    -- MEMBERSHIP rather than the per-keyword count: the rule asks only whether a
    -- player controls "a permanent with start your engines!", so a second
    -- instance can start no second set of engines.
    --
    -- Rule 702.178's max speed is deliberately NOT a sibling constructor here.
    -- CR 702.178a spells it as "as long as your speed is 4, this object has
    -- '[Ability]'", so the keyword would have to carry a whole ability, which
    -- would make this type parametric in `card` and drag Filter and Cost -- both
    -- parameterised by THIS type -- along with it. pawl spells that gate as
    -- Pawl.Types.ActivatedAbility.condition instead, which is the same "as long
    -- as" clause Pawl.Types.StaticAbility.condition already carries for CR 604.2.
    StartYourEngines
  deriving (Eq, Ord, Show)

-- Devoid takes TWO routes, decided by where the instance came from. A PRINTED one
-- is a characteristic-defining ability and is folded at the start of layer 5
-- (Projection.applyColorDefining), per CR 613.3. A GRANTED one is not, CR 604.3a
-- denying CDA status to an ability that is not printed on the card it affects, so
-- Projection.grantedDevoidParts routes it into layer 5 as an ordinary colour
-- effect timestamped with the granting permanent (CR 613.7a). Slivdrazi
-- Monstrosity is the card that separates them, and Pawl.ColorSpec's "CR 613.7a a
-- granted devoid clears an OLDER 'in addition' colour" is the proof.
