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
  | -- | 702.70a: whenever this creature deals combat damage to a player, that
    -- player gets N poison counters. N rides the constructor, as Toxic's does.
    -- Unlike toxic, the N values are NOT summed: CR 702.70b says each instance
    -- triggers separately, so `Poisonous 1` twice is two abilities and two
    -- triggers -- which is what Pawl.Engine.Keyword.triggeredAbilitiesOf builds
    -- from the projection's per-keyword count.
    Poisonous Natural.Natural
  | -- | 702.80a: damage this source deals to a creature isn't marked on it;
    -- instead its controller puts that many -1/-1 counters on that creature.
    -- Nullary, because CR 702.80d makes multiple instances redundant -- so unlike
    -- poisonous and battle cry, no reader here takes a count.
    --
    -- Infect's (702.90) CREATURE half and nothing more: CR 120.3d names both
    -- keywords together, while CR 120.3a's life-loss exception names infect
    -- alone, so wither damage to a player is ordinary life loss.
    Wither
  | -- | 702.86a: whenever this creature attacks, defending player sacrifices N
    -- permanents. N rides the constructor, as Poisonous' does, and for the same
    -- reason: CR 702.86b says each instance triggers separately, so
    -- `Annihilator 1` twice is two abilities and two sacrifices rather than one
    -- ability for 2 -- which is what Pawl.Engine.Keyword.triggeredAbilitiesOf
    -- builds from the projection's per-keyword count.
    --
    -- The SECOND keyword rule 702 states as a triggered ability, after poisonous
    -- (702.70a) and before battle cry (702.91a), and minted the same way. What
    -- it adds to those two is the PLAYER: rule 702.86a names the defending
    -- player, whom CR 508.5 reads off what the creature is attacking -- so
    -- Pawl.Engine.Combat.declareAttackers computes it as the attack is declared
    -- and GameEvent.AttackerDeclared carries it, and the minted ability reads it
    -- back through the reserved Pawl.Engine.Binding.triggerPlayer slot.
    Annihilator Natural.Natural
  | Infect -- 702.90
  | -- | 702.91a: whenever this creature attacks, each other attacking creature gets
    -- +1/+0 until end of turn. The THIRD keyword rule 702 states as a triggered
    -- ability, after poisonous (702.70a) and annihilator (702.86a) and before
    -- prowess (702.108a), and so the third one
    -- Pawl.Engine.Keyword MINTS rather than merely consults. Nullary, because rule
    -- 702.91a takes no parameter -- and unlike flying's or lifelink's nullary
    -- siblings, its reader takes the per-keyword count rather than membership,
    -- since CR 702.91b gives it the multiplicity CR 702.70b gives poisonous.
    BattleCry
  | -- | 702.108a: whenever you cast a noncreature spell, this creature gets +1/+1
    -- until end of turn. The FOURTH keyword rule 702 states as a triggered
    -- ability, after poisonous (702.70a), annihilator (702.86a) and battle cry
    -- (702.91a), and minted the same way. What it adds to those three is the
    -- WATCHED EVENT: the first of them to trigger on something other than its
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
  | Devoid -- 702.114
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
