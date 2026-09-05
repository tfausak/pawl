module Pawl.Types.Filter where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.RecipientKind as RecipientKind
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Zone as Zone

-- | A first-order predicate over one candidate -- an object, or (CR 115.1) a
-- player, since a target may be either -- expressed as data and evaluated by one
-- generic matcher (Pawl.Engine.Filter.matches) that never learns which effect
-- produced it. Its atoms case on CHARACTERISTICS, and on a handful of things CR
-- 109.3 says are not characteristics but that the closed half owns just as
-- squarely: combat status, attachment, what a permanent is represented by, and
-- what happened earlier this turn.
--
-- Flat, not layered: the atoms and the And/Or/Not combinators are sibling arms of
-- one type, and `And []` is the trivial predicate.
--
-- PARAMETRIC in the keyword, and only to break a module cycle: HasKeyword below
-- names Pawl.Types.Keyword, and that module already names this one.
data Filter keyword
  = HasCardType CardType.CardType -- CR 205 / 300: the object's card types include this one.
  | HasSupertype Supertype.Supertype -- CR 205.4: the object's supertypes include this one.
  | HasColor Color.Color -- CR 105.2: the object's colours include this one.
  | HasSubtype Subtype.Subtype -- CR 205.3: the object's subtypes include this one.
  | -- | CR 201.2: the object's names include this LITERAL one. Membership, as CR
    -- 709.4a states the test -- a split card off the stack and a Room with both
    -- doors unlocked each show two names at once. The printed name, never a
    -- chosen one, which is HasChosenName below.
    HasName CardName.CardName
  | -- | CR 702.1: the object HAS this keyword ability. Membership of the WRITTEN
    -- instance, so `HasKeyword (Toxic 2)` asks about toxic 2 specifically and
    -- Quagmire's swampwalk does not reach islandwalk; HasKeywordFamily below is
    -- the other question, and Pawl.FilterSpec pins the pair apart. Read through
    -- the CR 613 projection, in every zone.
    HasKeyword keyword
  | -- | CR 702.1: the object has SOME keyword ability of this family, whatever
    -- its payload -- toxic 1 and toxic 3 alike (CR 702.164a). A sibling of
    -- HasKeyword above rather than a widening of it, and answered off the
    -- projection for that atom's reason.
    HasKeywordFamily KeywordFamily.KeywordFamily
  | PowerAtLeast Integer -- CR 208.1: the object's power is >= this literal.
  | -- | CR 208.1 in the other direction: the object's power is <= this literal.
    -- Not PowerAtLeast's negation -- both arms answer False for an absent power,
    -- so neither is reachable from the other, and Pawl.FilterSpec pins that pair
    -- apart. Answered off the projection, in every zone.
    PowerAtMost Integer
  | -- | CR 208.1: the object's toughness is strictly greater than its power.
    ToughnessGreaterThanPower
  | -- | CR 208.1 compared against the SOURCE rather than a literal: the object's
    -- power is strictly less than the power of the object the evaluation comes
    -- from (CR 702.134a's mentor). Read off Pawl.Engine.Filter.Context's
    -- sourcePower, and vacuously False where either power is absent.
    PowerLessThanSource
  | -- | CR 208.1 compared against the SOURCE the other way: the object's power is
    -- strictly greater than the source's (CR 702.149a's training). Not
    -- PowerLessThanSource's negation, which admits equal and absent power alike.
    PowerGreaterThanSource
  | -- | CR 208.1 compared against a number an earlier clause of the same
    -- resolution BOUND at this slot -- Localized Destruction's "power equal to
    -- the amount of {E} paid this way". Exact equality, and vacuously False where
    -- either number is absent.
    PowerIsAmountInSlot SlotName.SlotName
  | -- | CR 208.1 against the same kind of bound number, one comparison over:
    -- the object's power is >= the amount an earlier clause of the resolution
    -- bound at this slot -- Valiant Endeavor's "power greater than or equal to
    -- that result". PowerAtLeast's comparison with PowerIsAmountInSlot's right
    -- operand, and vacuously False where either number is absent.
    PowerAtLeastAmountInSlot SlotName.SlotName
  | -- | CR 202.3: the object's mana value is <= this literal. Answerable off the
    -- battlefield, rule 202.3 reading the printed mana cost.
    ManaValueAtMost Integer
  | -- | CR 202.3 read for its PARITY rather than against a bound -- Void
    -- Winnower's "spells with even mana values", whose reminder text settles the
    -- boundary case: "(Zero is even.)" CR 202.3e is what makes it interesting off
    -- the stack, where an {X} in a cost counts as zero.
    ManaValueIsEven
  | -- | CR 202.3 compared against a COMPUTED bound: the object's mana value is <=
    -- the amount the enclosing target slot names (Pawl.Types.TargetSlot's
    -- @amount@), read again at CR 608.2b rather than frozen at announcement.
    -- Vacuously False where either number is absent.
    ManaValueAtMostAmount
  | ControlledBy PlayerRelation.PlayerRelation -- CR 109.5 / 102.2: controller relates thus to the perspective.
  | -- | CR 508.5: the candidate's controller is the DEFENDING PLAYER for the
    -- object the evaluation comes from (CR 702.39a's provoke). Not @ControlledBy
    -- Opponent@, which on a three-seat board admits a creature controlled by an
    -- opponent who is not being attacked at all. Vacuously False where the source
    -- has no defending player.
    ControlledByDefendingPlayer
  | -- | CR 603.2: the candidate's controller is the PLAYER BOUND at this slot --
    -- Trygon Predator's "that player", the one the trigger's own event named.
    -- Answered by rewriting (Pawl.Engine.Filter.bakeBound), and vacuously False
    -- if it survives to a match.
    ControlledByBound SlotName.SlotName
  | -- | The atom above with its player resolved: the candidate's controller IS
    -- this player. Runtime-only -- Pawl.CardSpec lints the pool against a card
    -- authoring one, a baked player in printed text being meaningless (#199).
    ControlledByPlayer PlayerId.PlayerId
  | -- | The candidate's controller is the player the surrounding effect is
    -- CURRENTLY BEING APPLIED TO -- Biorhythm's "the number of creatures THEY
    -- control". Not CR 109.5's "you", which is the spell's controller, and
    -- vacuously False outside a per-recipient effect's quantity.
    ControlledByRecipient
  | -- | CR 108.3 / 110.2: the candidate's OWNER relates thus to the perspective.
    -- A sibling of ControlledBy above and never derivable from it, and answerable
    -- in every zone where that atom is not: an owner is fixed when the game
    -- starts, while CR 108.4 gives a card outside the battlefield and the stack
    -- no controller. False off an object, a printed card being none (CR 109.1).
    OwnedBy PlayerRelation.PlayerRelation
  | -- | The candidate IS the evaluation's source object. @Not IsSource@ is how CR
    -- 601.2c's "another" and a continuous effect's own "each other" card text are
    -- both written -- one relation, one spelling (#163).
    IsSource
  | -- | CR 115.1: the candidate, a stack object, has the evaluation's source
    -- among its CR 601.2c targets (Terror of the Peaks' "spells ... that target
    -- this creature"); IsSource's posture, vacuously False off the stack.
    TargetsSource
  | -- | CR 115.1 the atom above narrowed by "only": the candidate, a stack
    -- object, targets the evaluation's source and nothing else (Zada, Hedron
    -- Grinder's "an instant or sorcery spell that targets only Zada").
    -- Vacuously False where the candidate targets nothing, a player target
    -- alone being enough to fail it.
    TargetsOnlySource
  | -- | CR 115.1 the atom above by KIND rather than by identity: the candidate, a
    -- stack object, targets exactly one thing, and that one target is a recipient
    -- of this kind (Ivy, Gleeful Spellthief's "a spell that targets only a single
    -- creature"). Vacuously False where the candidate targets nothing.
    --
    -- The KIND is the tag Pawl.Types.Pool minted at CR 601.2c, so a card that
    -- says "creature" is matched only where the spell's own slot named CR 115.1a's
    -- creature pool: a "target permanent" spell aimed at a creature carries
    -- Recipient.ToObject and is passed over. Stricter than printed, and stated as
    -- a limit of the tag rather than of the rule.
    --
    -- Not implemented: a Filter over the target itself, which "a single creature
    -- you control" (Frontline Heroism) and "a single Golem" (Precursor Golem)
    -- want and this module cannot see, holding no board (gap #3272).
    TargetsOnlyOne RecipientKind.RecipientKind
  | -- | CR 115.1 / 115.10a: the candidate has a player in this relation to the
    -- perspective among its targets, a ToPlayer alone counting (Shell of the
    -- Last Kappa's "spell that targets you").
    TargetsPlayer PlayerRelation.PlayerRelation
  | -- | The candidate IS an object the resolution bound in this slot -- Into the
    -- Wilds' "if it's a land card", where the clause before it looked at the top
    -- card of the library. Membership where CR 115.10a's group binding names
    -- several, and vacuously False where the slot names no object.
    IsBound SlotName.SlotName
  | -- | CR 201.2 / 709.4a asked of TWO objects: the candidate shares a name with
    -- the object this slot holds -- Harness the Storm's "target card with the
    -- same name as that spell". Set intersection, so an object showing two names
    -- has the same name as one showing either; vacuously False where the slot
    -- names no object or the bound object has no name (CR 708.2a).
    SameNameAsBound SlotName.SlotName
  | -- | CR 110.2 asked of TWO objects: the candidate has the same controller as
    -- the object this slot holds -- Bioshift's "another target creature with the
    -- same controller".
    --
    -- VACUOUSLY TRUE where the slot names no object, alone among these atoms: CR
    -- 601.2c's offer is made before either target is chosen, so
    -- Pawl.Engine.Target.legalSetsGiven widens and selectionLegal's joint check
    -- narrows, exactly as CR 608.2b will at resolution. Pawl.CardSpec's "CR 110.2
    -- no card asks SameControllerAsBound outside a mode's target slot" is what
    -- keeps the atom out of the positions with no joint check behind them.
    SameControllerAsBound SlotName.SlotName
  | -- | CR 201.4: the candidate has a name the SOURCE has chosen earlier in the
    -- same resolution (CR 608.2c) -- Ancient Vendetta's "cards with that name".
    -- Set intersection, for CR 201.4g's interchangeable names as much as CR
    -- 709.4a's, and vacuously False where the source has chosen no name.
    HasChosenName
  | -- | CR 702.16k: the candidate is an object the protection carrier's chosen
    -- player controls, or one they own that no other player controls.
    OfChosenPlayer
  | -- | CR 115.1: the candidate is a PLAYER who relates thus to the perspective
    -- -- "target opponent". Separate from ControlledBy, which asks who controls
    -- an object candidate rather than who the candidate is (CR 109.1, CR 108.4).
    IsPlayer PlayerRelation.PlayerRelation
  | -- | The candidate PLAYER is the controller of the object a slot names --
    -- Spikeshell Harrier's "each OTHER player". Answered by rewriting at
    -- Pawl.Engine.Count.bakePerspective, and vacuously False outside a
    -- Scope.OverPlayers count's filter.
    IsControllerOfBound SlotName.SlotName
  | -- | CR 110.2: the candidate PLAYER controls strictly more permanents matching
    -- this filter than the perspective player does (CR 109.5) -- Oreskos
    -- Explorer's "the number of players who control more lands than you".
    -- Answered by rewriting at Pawl.Engine.Count.bakePerspective, and vacuously
    -- False outside a Scope.OverPlayers count's filter. Strict, which is what
    -- lets Oreskos ask it of every player.
    --
    -- Not implemented: Surveyor's Scope's "at least two more lands than you",
    -- which wants a margin beside the filter (#2353).
    ControlsMoreThanYou (Filter keyword)
  | -- | CR 400.1: the candidate PLAYER's own graveyard holds at least this many
    -- cards -- The Master of Lake-town's "each graveyard with seven or more cards
    -- in it". A player atom like the two above, answered by
    -- Pawl.Engine.Count.bakePerspective and vacuously False if it survives to
    -- Pawl.Engine.Filter.matches.
    CardsInGraveyardAtLeast Natural.Natural
  | -- | CR 508.1k: the candidate is an ATTACKING creature, not since removed from
    -- combat (CR 506.4). Reading combat status breaches nothing: it is a rules
    -- concept the closed half owns, so the read is the same kind of act as
    -- reading a card type.
    IsAttacking
  | -- | CR 508.1b: the candidate is an attacking creature AND the player it is
    -- attacking stands in this relation to the perspective (CR 109.5). Strictly
    -- CR 508.1b's player and never CR 508.5's defending player, which CR 509.1a
    -- and CR 802.4a spell apart as three subjects -- a creature attacking a
    -- planeswalker you control does not match @IsAttackingPlayer You@. Present
    -- tense, so CR 506.4's removal from combat makes it False.
    IsAttackingPlayer PlayerRelation.PlayerRelation
  | -- | CR 508.1b: the candidate is an attacking creature AND the CONTROLLER of
    -- the planeswalker it is attacking stands in this relation to the perspective
    -- (CR 109.5), the second subject of CR 509.1a's and CR 802.4a's list.
    -- Pawl.CombatEffectSpec's "CR 508.1b whole card: Soul Snare reaches a creature
    -- attacking a planeswalker you CONTROL" is what proves it reads the controller
    -- and not CR 108.3's owner.
    IsAttackingPlaneswalker PlayerRelation.PlayerRelation
  | -- | CR 508.1b: the candidate is an attacking creature AND the PROTECTOR of
    -- the battle it is attacking stands in this relation to the perspective (CR
    -- 109.5), the third subject of CR 509.1a's and CR 802.4a's list.
    -- Pawl.BattleSpec's "CR 310.9d whole card: Synthetic Bulwark Snare reaches a
    -- creature attacking a battle you PROTECT" is what proves it reads CR 310.9d's
    -- protector and not the battle's controller.
    IsAttackingBattle PlayerRelation.PlayerRelation
  | -- | CR 508.3b: the candidate -- a player, planeswalker or battle -- was
    -- DECLARED ATTACKED this combat phase, the mirror of
    -- DeclaredAttackerThisCombat below. A look-back read of
    -- Combat.declaredAttacked, so CR 508.4's creature put onto the battlefield
    -- attacking never makes it True and CR 506.4's removal never makes it False;
    -- CR 511.3 clears the record with the rest of the combat phase's.
    DeclaredAttackedThisCombat
  | -- | CR 509.1g: the candidate is a BLOCKING creature, not since removed under
    -- CR 506.4. Not the question IsBlocked below asks: CR 509.1h keeps a creature
    -- blocked once every blocker has left combat.
    IsBlocking
  | -- | CR 509.1h: the candidate is a BLOCKED creature -- an attacking creature
    -- one or more creatures were declared blocking, or that an effect said
    -- becomes blocked. "Unblocked" is @Not IsBlocked@ (#163).
    IsBlocked
  | -- | CR 508.1a: the candidate was DECLARED as an attacker THIS COMBAT PHASE,
    -- read off Combat.declaredAttackers, which CR 511.3 clears. Not IsAttacking:
    -- CR 506.4a leaves a removed attacker's declaration standing, and CR 508.1k
    -- makes the chosen creatures attacking only after CR 508.1j's payment.
    DeclaredAttackerThisCombat
  | -- | CR 509.1a: the candidate was DECLARED as a blocker THIS COMBAT PHASE. Not
    -- IsBlocking, for the atom above's reasons with CR 509.1g in place of CR
    -- 508.1k -- CR 509.1f pays while CR 509.1g has not run, so a fellow creature
    -- chosen in the same declaration is not blocking yet.
    DeclaredBlockerThisCombat
  | -- | CR 608.2i: the candidate was DECLARED as an attacker earlier this turn --
    -- Relentless Assault's "all creatures that attacked this turn". A look-back
    -- read of the turn-scoped GameEvent log, which outlives CR 511.3's wipe of
    -- Combat.attackers; declared, so CR 508.4's creature put onto the battlefield
    -- attacking never attacked.
    AttackedThisTurn
  | -- | CR 701.17a: the candidate is a card that was MILLED earlier this turn --
    -- The Master, Transcendent's "creature card in a graveyard that was milled
    -- this turn". AttackedThisTurn's look-back read one event arm over
    -- (GameEvent.Milled), and not "moved from a library to a graveyard", which
    -- surveil (CR 701.25a) and explore (CR 701.44a) also do. Matches the
    -- incarnation the mill left the card as (CR 400.7).
    MilledThisTurn
  | -- | CR 120.1 / 608.2i: the candidate -- an object or a player, since CR 120.1
    -- has damage dealt to both -- was DEALT DAMAGE earlier this turn.
    -- AttackedThisTurn's look-back read one event arm over
    -- (GameEvent.DamageDealt), and not Object.damage being positive: a wither or
    -- infect source marks nothing at all (CR 120.3d).
    DealtDamageThisTurn
  | -- | CR 303.4b / 701.3a: the candidate is ATTACHED to something the nested
    -- Filter admits -- Crown of the Ages' "target Aura attached to a creature".
    -- The nest asks about the HOST's characteristics against the OUTER context,
    -- so @AttachedTo (ControlledBy You)@ is "attached to something you control".
    -- False for a candidate attached to a player, and for one whose host has left
    -- the battlefield (CR 704.5m).
    AttachedTo (Filter keyword)
  | -- | CR 303.4b's "enchanted" and CR 301.5a's "equipped", asked of the HOST:
    -- something the nested Filter admits is attached TO the candidate -- A Tale
    -- for the Ages' "enchanted creatures you control". The mirror of AttachedTo
    -- above, neither expressing the other, attachment being directed. Any, not
    -- all (CR 303.4b), and narrowed to attachers on the battlefield.
    --
    -- CR 303.4b's other enchantable is a PLAYER candidate: vacuously False where
    -- this reaches a match unbaked, ControlsMoreThanYou's posture, since a bare
    -- player View holds no board to find the attachers on. Answered by rewriting
    -- at Pawl.Engine.Count.bakePerspective.
    HasAttached (Filter keyword)
  | -- | CR 701.3a / 301.5a: the candidate is attached to the evaluation's SOURCE
    -- -- Kemba's Legion's "for each Equipment attached to this creature". Host
    -- IDENTITY and not a host quality, so it reads an ObjectId off the candidate's
    -- own view; vacuously False where the candidate is attached to nothing or to
    -- a player, and where no source frames the match.
    IsAttachedToSource
  | -- | CR 303.4b's "enchanted": the candidate is what the evaluation's SOURCE is
    -- attached to -- Ray of Frost's "enchanted creature", the third attachment
    -- direction. Vacuously False where the source is attached to nothing or to a
    -- player; Pawl.CardSpec's position lint keeps a card to the positions that
    -- fill the field.
    IsHostOfSource
  | -- | CR 701.3a's last sentence: the candidate is one the SUBJECT of the
    -- surrounding attach could legally be attached to -- Aura Graft's "another
    -- permanent IT CAN ENCHANT". Answered by Pawl.Engine.Attach.attachmentFor, so
    -- CR 303.4's other limits arrive with the subject's own enchant ability --
    -- Pawl.AuraSpec's "Aura Graft will not move an Aura onto a land Consecrate
    -- Land protects" is what proves the two arrive together. Pawl.CardSpec
    -- rejects the atom outside an attach's destination.
    CanHostSubject
  | -- | CR 701.3a from the other side: could THIS CANDIDATE legally be attached
    -- to the object the surrounding instruction fixes? Auratouched Mage's "an
    -- Aura card that could enchant it", the mirror of CanHostSubject above.
    -- Answered by Pawl.Engine.Attach.attachableWithLastKnown, whose other half is
    -- CR 608.2h's last-known read of a host that has left the battlefield;
    -- Pawl.CardSpec rejects the atom outside a search's filter.
    --
    -- Not implemented: the same question with the host fixed by anything but the
    -- searching ability's source -- Sovereigns of Lost Alara's bound creature,
    -- Bruna's, and Takklemaggot's choose-position reading of CanHostSubject
    -- (#2028).
    CanAttachToSubject
  | -- | CR 111.6: the candidate is a token; "nontoken" is @Not IsToken@ (#163).
    -- Uncharacteristic and immutable, CR 111.3 making a token's effect-defined
    -- values equivalent to printed ones, which is what lets
    -- Pawl.Engine.Projection.filterReads declare it as reading nothing.
    IsToken
  | -- | CR 113.3b: the candidate is an ACTIVATED ability on the stack, not CR
    -- 113.3c's triggered one -- Squelch's "target activated ability".
    -- Uncharacteristic and immutable for IsToken's reason.
    IsActivatedAbility
  | -- | CR 113.7 / 113.7a: the candidate is an ability on the stack whose SOURCE,
    -- read with last known information, matches the nested filter (Green Slime).
    FromSource (Filter keyword)
  | -- | CR 110.5: the candidate is tapped; "untapped" is @Not IsTapped@ (#163). A
    -- STATUS rather than a characteristic, so nothing in CR 613 projects it.
    IsTapped
  | -- | CR 110.5: the candidate is a FACE-DOWN permanent -- the other value of
    -- the same status category IsTapped reads one of, and "face up" is @Not
    -- IsFaceDown@ (#163). Narrowed to the battlefield by the atom, since CR
    -- 110.5d gives only permanents status and CR 708.4 turns an object face down
    -- before it reaches the stack. Never CR 110.5d's face-down EXILED card, which
    -- IsExiledFaceDown below reads.
    IsFaceDown
  | -- | CR 708.12: the CARD REPRESENTING the candidate matches the nested filter,
    -- read off that card's printed face with nothing in CR 613 applied
    -- (Hauntwoods Shrieker's "if it's a creature card"). A plain HasCardType
    -- would be vacuous, CR 708.2a leaving every face-down permanent a 2/2
    -- creature; Pawl.FaceDownSpec's Hauntwoods Shrieker group is the pair of
    -- boards that separates them.
    RepresentedByCard (Filter keyword)
  | -- | CR 406.3: the candidate is a card that was exiled FACE DOWN; "face-up
    -- exiled card" is @Not IsExiledFaceDown@ (#163). A separate atom from
    -- IsFaceDown above, which CR 110.5d says in as many words.
    --
    -- Not CR 406.4's permission to look, which the rules core answers instead
    -- (Pawl.Engine.Target.piledOffer): Pawl.ExileSpec's Augury Raven group proves
    -- a player offered their own foretold card by that rule is still refused it
    -- by Riftsweeper's own words.
    IsExiledFaceDown
  | -- | CR 701.27g: the candidate is a "transformed permanent" -- a double-faced
    -- permanent on the battlefield with its back face up. Both of the rule's
    -- exclusions are in the answer: a permanent showing its front face is never
    -- transformed however it turned before, and neither is one represented by
    -- more than one card.
    --
    -- Not implemented: CR 730.3's merged permanent, the second exclusion's other
    -- half (#874).
    Transformed
  | -- | CR 701.54e: the candidate "is your Ring-bearer", asked of the Context's
    -- perspective (CR 109.5). ONE of the rule's three conjuncts -- "on the
    -- battlefield under your control" is spelled by the surrounding set.
    -- Uncharacteristic, CR 701.54b making Ring-bearer a designation rather than a
    -- copiable value.
    IsRingBearer
  | -- | Does the CANDIDATE have this designation? Aragorn, Hornburg Hero's
    -- "renowned creature you control" and Rune-Brand Juggler's "suspected
    -- creature". Not Pawl.Types.Quantity.HasDesignation, which asks the same
    -- designation of the object an evaluation is aimed at, and not what CR
    -- 701.60c hangs off a designation. Uncharacteristic for IsRingBearer's reason.
    HasDesignation Designation.Designation
  | -- | CR 122.1: does the CANDIDATE have one or more counters of this kind on
    -- it? "One or more" and not a count, which is what every printing asks.
    -- Uncharacteristic: CR 109.3's list has no counters in it, and the P/T a
    -- +1/+1 counter grants is CR 613.4c's, applied over the top of this.
    HasCounters (CounterKind.CounterKind keyword)
  | -- | CR 122.1 again, kind-agnostic: does the CANDIDATE have one or more
    -- counters on it of ANY kind? Not spellable as an Or over the kinds, CR
    -- 122.1b's keyword counters making CounterKind unenumerable, and a separate
    -- nullary atom rather than a Maybe payload on HasCounters above, see #994.
    HasCountersOfAnyKind
  | -- | CR 602.1 / 605.1a: does the CANDIDATE have one or more activated
    -- abilities that aren't mana abilities? Tsabo's Web's "each land with an
    -- activated ability that isn't a mana ability". The abilities it reads are
    -- the ones the object HAS, which CR 702.29b and CR 702.77b keep in existence
    -- outside the zone they can be activated from. Characteristic, unlike the
    -- atoms above it: CR 613.1f writes abilities, so Humility stops it matching.
    HasNonManaActivatedAbility
  | -- | CR 602.1: does the CANDIDATE have one or more activated abilities, mana
    -- abilities among them? Zirda, the Dawnwaker's companion condition -- "each
    -- permanent card in your starting deck has an activated ability" -- is the
    -- card asking, and the atom above is the same question with CR 605.1a's
    -- exclusion, which Zirda does not write.
    HasActivatedAbility
  | -- | CR 400.1: the candidate OBJECT is in this zone; "from anywhere other than
    -- their hands" is @Not (IsInZone Hand)@ (#163). A cast gate reads it before
    -- CR 601.2a moves the card to the stack, which is what CR 601.2's "take it
    -- from where it is" asks for. Names a zone and not whose -- "in your
    -- graveyard" is an OwnedBy conjunct beside this atom. Vacuously False where
    -- there is no OBJECT to ask (CR 109.1).
    IsInZone Zone.Zone
  | -- | CR 601.2a: which zone the candidate SPELL was moved to the stack from --
    -- Patrician Geist's "spells you cast from your graveyard". Remembered in
    -- Pawl.Types.Object.castFrom precisely because CR 400.7 leaves the spell no
    -- memory of it, so it is never IsInZone above, which reads where the object
    -- is now. Vacuously False for everything that was never cast.
    WasCastFrom Zone.Zone
  | And [Filter keyword]
  | Or [Filter keyword]
  | Not (Filter keyword)
  deriving (Eq, Ord, Show)
