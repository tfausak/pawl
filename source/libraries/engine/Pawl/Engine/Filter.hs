module Pawl.Engine.Filter where

import qualified Data.Functor.Const as Const
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.NameWords as NameWords
import qualified Pawl.Types.Behold as Behold
import qualified Pawl.Types.BoundMeasure as BoundMeasure
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CountersFromPermanents as CountersFromPermanents
import qualified Pawl.Types.Craft as Craft
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.Devour as Devour
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.Emerge as Emerge
import qualified Pawl.Types.Equip as Equip
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.ExileMaterials as ExileMaterials
import qualified Pawl.Types.ExilePermanents as ExilePermanents
import qualified Pawl.Types.Expansion as Expansion
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ForetellCost as ForetellCost
import qualified Pawl.Types.Impending as Impending
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.KeywordCount as KeywordCount
import qualified Pawl.Types.KeywordTally as KeywordTally
import qualified Pawl.Types.MadnessCost as MadnessCost
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Measure as Measure
import qualified Pawl.Types.Measures as Measures
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Operand as Operand
import qualified Pawl.Types.Pairing as Pairing
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.ProductionTag as ProductionTag
import qualified Pawl.Types.Protection as Protection
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.ReturnPermanents as ReturnPermanents
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SlotArity as SlotArity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Splice as Splice
import qualified Pawl.Types.StickerKind as StickerKind
import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.StoredResult as StoredResult
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.Suspend as Suspend
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower
import qualified Pawl.Types.TapPermanents as TapPermanents
import qualified Pawl.Types.Teams as Teams
import qualified Pawl.Types.Ward as Ward
import qualified Pawl.Types.WhichCounters as WhichCounters
import qualified Pawl.Types.Zone as Zone

-- The characteristics a Filter atom consults. Supplied by the projection on the
-- battlefield/stack and by the printed card off the battlefield (both builders
-- live in Pawl.Engine.Projection), or by `playerView` below when the candidate is a
-- player rather than an object. `controller` is Nothing off the
-- battlefield -- a card in a library has none under the rules that matter here
-- -- so ControlledBy is vacuously False there, which no search
-- filter uses. `owner` and `manaValue` are the two axes that do NOT go vacuous
-- with the zone, since CR 108.3 and CR 202.3 both name facts a card carries
-- everywhere; each field says so.
data View = MkView
  { -- CR 201.1 / 709.4a: the candidate's names, plural because an object does
    -- not have one -- the axis Pawl.Types.ProjectedCharacteristics.names already
    -- carries, brought across unchanged so that HasName asks the MEMBERSHIP rule
    -- 709.4a asks ("an object has the chosen name if one of its names is the
    -- chosen name") rather than comparing to a string.
    --
    -- Read off the CR 613 projection wherever there is an object, in any zone --
    -- rule 613.1 names none -- and off the printed face where the builder holds
    -- only a face. That is what lets a library search answer for a card that is
    -- not a permanent, which is where the pool's first reader looks
    -- (Asmoranomardicadaistinaculdacar).
    --
    -- EMPTY where there is nothing named to read: a player view, an ability on
    -- the stack (CR 113.7a), and a face-down object, whose CR 708.2a "no name"
    -- an empty set is the honest spelling of.
    names :: Set.Set CardName.CardName,
    cardTypes :: Set.Set CardType.CardType,
    supertypes :: Set.Set Supertype.Supertype,
    colors :: Set.Set Color.Color,
    subtypes :: Set.Set Subtype.Subtype,
    -- CR 702: the keyword abilities the candidate has. A SET and not the
    -- projection's Map Keyword Natural, because neither reader needs the count:
    -- HasKeyword asks membership, and HasKeywordFamily scans for a key whose
    -- family matches. Read from the PROJECTION on the battlefield and from the
    -- printed card off it, so a creature that gains flying at layer 6 matches and
    -- a Humility'd one (CR 613.1f) does not.
    keywords :: Set.Set Keyword.Type.Keyword,
    power :: Maybe Integer,
    -- CR 208.1: the candidate's toughness, read exactly as `power` above is and
    -- Nothing in exactly the same places -- a permanent with no toughness box, a
    -- player, a card outside the battlefield. No Filter atom consults it: it is
    -- here for Pawl.Engine.Quantity's Toughness arm, which reads a View like
    -- every other characteristic-reading quantity, and CR 702.100a's evolve is
    -- the pool's one reader.
    toughness :: Maybe Integer,
    -- Alchemy's intensity (Pawl.Types.ProjectedCharacteristics.intensity), read
    -- off the projection in every zone. No Filter atom consults it: it is here
    -- for Pawl.Engine.Quantity's Intensity arm.
    intensity :: Maybe Integer,
    -- CR 202.3: the candidate's mana value (CR 202.3a gives a costless object
    -- 0). On the battlefield it comes off the CR 613 projection, so CR 707.2's
    -- copiable mana cost is honoured -- a Clone reports what it copied. Off the
    -- battlefield it is the printed cost's, and unlike `power` it is NOT Nothing
    -- there -- a mana cost is printed on the card and rule 202.3 names no zone
    -- -- which is what lets a mana value comparison filter a graveyard.
    --
    -- Just for every OBJECT, an ability on the stack included: CR 109.1 makes
    -- one an object and CR 202.3a gives an object with no mana cost a 0
    -- (Pawl.CounterspellSpec's Synthetic Weigh the Trigger group). Nothing where
    -- there is no object at all -- a player view, or an event snapshot carrying
    -- none.
    manaValue :: Maybe Integer,
    -- CR 202.1: the candidate's mana cost itself, read off the same place
    -- `manaValue` above is and Nothing in the same places, plus one more -- CR
    -- 202.1b's land, which has no mana cost at all where it still has a mana
    -- value of 0.
    --
    -- Beside the value rather than instead of it, because CR 202.3 is not
    -- recoverable in reverse: {B}{B} and {1}{B} are both 2, and CR 700.5's
    -- devotion counts the SYMBOLS. Pawl.Engine.Quantity's Devotion arm is the one
    -- reader; no Pawl.Types.Filter atom asks about it.
    manaCost :: Maybe ManaCost.ManaCost,
    controller :: Maybe PlayerId.PlayerId,
    -- CR 108.3 / 110.2: the candidate's OWNER -- the player who started the game
    -- with the card in their deck, or (CR 111.2) the player who created the
    -- token. Read straight off Object.owner, which setup writes once and no rule
    -- ever rewrites: CR 613.1b's layer 2 changes CONTROL, and rule 108.3 has no
    -- counterpart, so no projection is consulted.
    --
    -- Just wherever `controller` above is, and in more besides, on an OBJECT
    -- view, and deliberately: an owner is a fact about a card IN THE GAME, so it
    -- is answerable in every zone, where CR 108.4 gives a card outside the
    -- battlefield and the stack no controller at all. That is manaValue's
    -- posture rather than power's, for manaValue's reason.
    --
    -- Not strictly more across every view: an event snapshot answers both off
    -- the arm rather than off an object (Count.viewOfSnapshot), and a snapshot
    -- whose object left no CR 608.2h record behind answers neither.
    --
    -- Nothing only where there is no OBJECT to read it off: a player view, an
    -- event snapshot, or a printed card being matched by a search, which CR
    -- 109.1 makes an object of nothing. OwnedBy is vacuously False there, the
    -- posture power and controller take.
    owner :: Maybe PlayerId.PlayerId,
    -- CR 400.1: which ZONE the candidate object is in -- what IsInZone reads.
    -- Off Object.zone rather than through any projection: CR 109.3 counts no zone
    -- among an object's characteristics, so no CR 613 layer writes one, exactly
    -- as `token` and `tapped` below are read off the object.
    --
    -- Nothing where there is no OBJECT to read it off -- a player view, an event
    -- snapshot, or a printed card being matched by a search, which CR 109.1 makes
    -- an object of nothing -- so IsInZone is vacuously False there, the posture
    -- `controller` and `identity` already take. Nothing too for an object
    -- outside the game (CR 400.11), which is in no zone.
    zone :: Maybe Zone.Zone,
    -- CR 601.2a: which zone the candidate was moved to the stack FROM when it was
    -- cast -- what WasCastFrom reads, off Pawl.Types.Object.castFrom.
    --
    -- A SECOND field beside `zone` above rather than a reading of it, and CR
    -- 400.7 is the whole reason: the spell is a new object with no memory of the
    -- zone it left, so nothing derives this from the board. `zone` answers Stack
    -- for every spell CR 601.2f prices.
    --
    -- Nothing for everything that was never cast -- a card at rest, a token, an
    -- ability on the stack -- and for everything with no OBJECT behind it at all,
    -- so WasCastFrom is vacuously False there, `zone` above's posture.
    castFrom :: Maybe Zone.Zone,
    -- CR 115.1: what the candidate TARGETS, for a spell or ability on the stack
    -- -- the recipients CR 601.2c (602.2b, 603.3d) chose, read live off the
    -- object's target-slot bindings by Pawl.Engine.Projection.View.targetsOfStackObject
    -- so that CR 115.7's change-of-target is seen at once. The union over the
    -- slots: TargetsSource and TargetsPlayer ask membership, never arity.
    --
    -- Empty for everything that targets nothing -- a permanent, a card at rest, a
    -- spell with no target slot -- and for everything with no object behind it,
    -- so both atoms are vacuously False there, `castFrom` above's posture. Empty
    -- too for a spell read at the CASTABILITY gate, which runs before CR 601.2c
    -- has chosen anything: Pawl.Engine.Cast.castProposed says what that costs.
    targets :: Set.Set Recipient.Recipient,
    -- CR 115.1 asked of the TARGETS rather than of the candidate: the view of
    -- each recipient above, so that TargetsOnlyOne's and TargetsMatching's nests
    -- have something to match against. Filled beside `targets` by
    -- Pawl.Engine.Projection.View.targetViewsOfStackObject, through the same
    -- bounded `peers` reader `attachedToView` below takes -- an object target
    -- answers that reader and a player target answers
    -- Pawl.Engine.Count.playerView.
    --
    -- LAZY on purpose: every stack object's view would otherwise project its
    -- targets whether or not any filter asks. Nothing forces this map but the
    -- atom that reads it.
    --
    -- A key of `targets` missing here is a target whose object is gone (CR
    -- 608.2b), and the atom answers False for it rather than guessing.
    targetViews :: Map.Map Recipient.Recipient View,
    -- CR 601.2c: how many targets the candidate has, an object counted once per
    -- instance of the word "target" that chose it -- the ARITY `targets` above
    -- forgets by taking the union. Filled beside it by
    -- Pawl.Engine.Projection.View.targetCountOfStackObject; zero wherever
    -- `targets` is empty.
    targetCount :: Natural.Natural,
    -- Which object this view is OF. Nothing for a printed card off the
    -- battlefield, which is not an object -- so IsSource is vacuously False
    -- there, the same posture power and controller already take.
    identity :: Maybe ObjectId.ObjectId,
    -- Which PLAYER this view is of, when the candidate is a player rather than
    -- an object (CR 115.1's "target opponent"). Nothing for every object view --
    -- so IsPlayer is vacuously False there, and every object-shaped field above
    -- is vacuously False on a player view. The two candidate kinds share one
    -- View type rather than splitting it, because Filter.matches folds And/Or/Not
    -- over whatever it is given and would otherwise need two trees.
    playerIdentity :: Maybe PlayerId.PlayerId,
    -- CR 508.1k: is this candidate an attacking creature right now? Not a
    -- characteristic (CR 109.3 says so in as many words), so it is read from
    -- GameState.combat rather than from a projection, or for an object that has
    -- left off CR 608.2h's record of it (Pawl.Engine.Count.lastKnownView). False
    -- for every candidate with no combat status to read: a printed card off the
    -- battlefield, a player, a cast's snapshot -- the vacuous posture power and
    -- controller take.
    attacking :: Bool,
    -- CR 508.1b: WHICH PLAYER is this candidate attacking? Read from
    -- GameState.combat like `attacking` above and off the same map, but off its
    -- VALUE rather than its keys: Combat.attackers records what each attacker was
    -- announced as attacking.
    --
    -- Nothing for every candidate `attacking` is False for, and ALSO for an
    -- attacking creature whose AttackTarget is a planeswalker or a battle. That
    -- narrowing is the point: CR 509.1a and CR 802.4a both keep "that player"
    -- apart from "a planeswalker they control", so this field is CR 508.1b's
    -- player and never CR 508.5's defending player (Pawl.Engine.Defender).
    attackingPlayer :: Maybe PlayerId.PlayerId,
    -- CR 508.1b: WHO CONTROLS the planeswalker this candidate is attacking? The
    -- same map's value as `attackingPlayer` above, read off its other arm and then
    -- followed to a controller -- so the two fields are disjoint by construction
    -- and each is CR 509.1a's own subject rather than CR 508.5's defending player.
    --
    -- The CONTROLLER, which is the seat CR 508.1b names and CR 613.1b's layer 2
    -- can move, and never CR 108.3's owner. Nothing for every candidate
    -- `attacking` is False for, and for an attacking creature whose AttackTarget
    -- is a player or a battle.
    attackingPlaneswalkerController :: Maybe PlayerId.PlayerId,
    -- CR 310.9d: WHO PROTECTS the battle this candidate is attacking? The same
    -- map's third arm, followed to the battle's protector -- so this and the two
    -- fields above are disjoint by construction and each is one of CR 509.1a's
    -- three subjects rather than CR 508.5's defending player.
    --
    -- The PROTECTOR, which CR 310.9d substitutes for the defending player while
    -- the battle is attacked, and never the battle's controller: CR 310.12a puts
    -- a Siege's protector among its controller's opponents, so the two seats
    -- differ on every Siege. Nothing for every candidate `attacking` is False
    -- for, for an attacking creature whose AttackTarget is a player or a
    -- planeswalker, and for a battle mid-repair with no designation (CR 310.11).
    attackingBattleProtector :: Maybe PlayerId.PlayerId,
    -- CR 508.3b: was this candidate DECLARED ATTACKED this COMBAT PHASE? Read
    -- from GameState.combat like `attacking`, off Combat.declaredAttacked -- the
    -- half of the record `declaredAttackerThisCombat` below reads the other half
    -- of. A LOOK-BACK read within the phase, so CR 506.4's removal from combat
    -- leaves it standing.
    --
    -- The one combat field a PLAYER candidate can answer, which is CR 508.3b's
    -- own subject list ("a player, planeswalker, or battle"). Filled from the
    -- board by Pawl.Engine.Count.playerView for that candidate, exactly as
    -- `dealtDamageThisTurn` below is and for its reason: this builder holds a
    -- PlayerId and no board.
    declaredAttackedThisCombat :: Bool,
    -- CR 509.1g: is this candidate a blocking creature right now? Read from
    -- GameState.combat alongside `attacking` -- but from the OTHER map:
    -- Combat.blockers is keyed by attacker, and a blocking creature is a MEMBER
    -- of some attacker's set.
    blocking :: Bool,
    -- CR 509.1h: is this candidate a BLOCKED creature right now? Read from
    -- GameState.combat like the two above, and off the same map `blocking`
    -- reads -- but from its KEYS, which is Pawl.Engine.Combat.isBlocked's
    -- question and never `blocking`'s.
    blocked :: Bool,
    -- CR 509.1g: the creatures blocking this candidate right now, the member set
    -- Combat.blockers keeps under its key. Empty wherever `blocked` is False.
    blockers :: Set.Set ObjectId.ObjectId,
    -- CR 608.2i: was this candidate declared as an attacker earlier this turn?
    -- Unlike `attacking` not even a present state: it is a look-back read of the
    -- turn-scoped GameEvent log.
    --
    -- LAZY, like `attachedToView` below but for a plainer reason: filling it
    -- folds the whole turn's event log, and nothing forces it unless a Filter
    -- actually contains AttackedThisTurn. That is a cost argument rather than
    -- the recursion hazard that field records.
    attackedThisTurn :: Bool,
    -- CR 508.1a: was this candidate DECLARED as an attacker this COMBAT PHASE?
    -- Read from GameState.combat like `attacking`, and off a different field of
    -- it: CR 506.4 removal ends the attacking and CR 506.4a leaves the
    -- declaration standing, and CR 508.1k writes `attacking` only after CR
    -- 508.1j's payment while this is written before it.
    declaredAttackerThisCombat :: Bool,
    -- CR 509.1a: the same question on CR 509's side, and the same relationship
    -- to `blocking` that the field above has to `attacking` -- with CR 509.1g in
    -- place of CR 508.1k.
    declaredBlockerThisCombat :: Bool,
    -- CR 701.17a: was this candidate MILLED earlier this turn? A look-back read
    -- of the same log `attackedThisTurn` above reads, and LAZY for that field's
    -- reason -- nothing forces it unless a Filter contains MilledThisTurn.
    milledThisTurn :: Bool,
    -- CR 120.1 / 608.2i: was this candidate DEALT DAMAGE earlier this turn? The
    -- same log the two fields above read, and LAZY for their reason -- nothing
    -- forces it unless a Filter contains DealtDamageThisTurn.
    --
    -- Deliberately not Object.damage: CR 120.6 removes marked damage on a
    -- regeneration, CR 701.69a heals it away and CR 120.3d/120.3e mark none at
    -- all for a wither or infect source, so the marks are a strict subset of
    -- what was dealt.
    dealtDamageThisTurn :: Bool,
    -- CR 400.7 / 608.2i: did this candidate, as the object it is now, enter the
    -- battlefield earlier this turn? The same log again, and LAZY for its reason
    -- -- nothing forces it unless a Filter contains EnteredThisTurn.
    enteredThisTurn :: Bool,
    -- CR 702.122c: which objects did this candidate crew earlier this turn? The
    -- same log the three fields above read, and LAZY for their reason -- nothing
    -- forces it unless a Filter contains CrewedSourceThisTurn.
    --
    -- The VEHICLES rather than a Bool, because rule 702.122c's relation has two
    -- ends: this half is the candidate's, and the other end -- which Vehicle the
    -- card means by "it" -- is the source on the Context, which the builders that
    -- fill this field do not hold.
    crewedThisTurn :: Set.Set ObjectId.ObjectId,
    -- CR 702.51c: which spells did this candidate convoke earlier this turn, and
    -- which permanents did those spells become? The same log the fields above
    -- read, and LAZY for their reason -- nothing forces it unless a Filter
    -- contains ConvokedSourceThisTurn.
    --
    -- The two ends of rule 702.51c's relation are split the way
    -- `crewedThisTurn` above splits rule 702.122c's: this half is the
    -- candidate's, and the "it" the card names is the source on the Context. The
    -- PERMANENTS are in the set beside the spells because a spell is gone by the
    -- time its own entry trigger resolves (CR 400.7d).
    convokedThisTurn :: Set.Set ObjectId.ObjectId,
    -- CR 702.171c: which Mounts did this candidate saddle earlier this turn?
    -- `crewedThisTurn` above's shape and laziness, one keyword over, and a field
    -- of its own so a saddler never satisfies CrewedSourceThisTurn.
    saddledThisTurn :: Set.Set ObjectId.ObjectId,
    -- CR 302.6: has this candidate's CONTROLLER controlled it continuously since
    -- their most recent turn began? Read from Object.sickness, the field CR
    -- 302.6's own gates on attacking and the tap symbol are read from, and
    -- compared against the PROJECTED controller so that layer 2 moving the seat
    -- and Pawl.Engine.Engine.checkControlContinuity clearing the settle agree.
    controlledSinceTurnBegan :: Bool,
    -- CR 303.4 / 110.1 / 701.3a: the HOST this candidate is attached to, viewed
    -- as a candidate in its own right, so that AttachedTo's nested Filter has
    -- something to be evaluated against. Not a characteristic either (CR 109.3):
    -- the attachment comes off Object.attachedTo, and only the host's half of
    -- the answer is projected.
    --
    -- Nothing where the candidate is attached to nothing, to a PLAYER (CR
    -- 303.4's other destination, which Recipient.objectOf rejects), or to an
    -- object that is no longer on the battlefield and so is no longer a
    -- permanent -- the stale window CR 704.5m closes on the next
    -- state-based-action pass.
    --
    -- Recursive, and therefore LAZY. Deciding Just from Nothing costs no
    -- projection at all; only reaching INSIDE the host's view does, and
    -- Projection.viewOfCharacteristics is itself called from inside
    -- Projection.affectsGiven while a projection is being computed. So
    -- `AttachedTo (And [])` forces nothing beyond the attachment, and a nest that
    -- names a characteristic forces exactly one further projection per link.
    --
    -- Laziness is the COST argument and not the termination one. What makes a
    -- forced nest terminate is that the builder is handed a reader bounded at
    -- its caller's own depth (Projection.viewOfCharacteristics' `hosts`), so a
    -- nest reached from inside the fold reads the host through that fold's
    -- layers rather than re-entering `gather`. A nest inside a nest descends the
    -- filter, which is finite, so an attachment CYCLE terminates too.
    --
    -- View has no derived Eq, Ord or Show, and this field is why: CR 303.4 lets
    -- an effect momentarily produce a cycle of attachments before CR 704.5m's
    -- pass, and a structural walk would not terminate on one. Nothing needed
    -- them -- no type embeds a View, so no derived instance depended on them
    -- either.
    attachedToView :: Maybe View,
    -- CR 303.4b / 301.5a read the other way: the permanents attached TO this
    -- candidate, each viewed as a candidate in its own right, so that
    -- HasAttached's nested Filter has something to be evaluated against. The
    -- reverse of the index `attachedToView` above reads, and gathered by sweeping
    -- the battlefield for the permanents whose Object.attachedTo names this one --
    -- pawl stores the attachment on the ATTACHED permanent, so there is nothing to
    -- look up from this side.
    --
    -- A LIST rather than a Set or a Seq. View has no Eq or Ord (see
    -- `attachedToView`), so a Set is not available; and a list is lazy in its
    -- SPINE as well as its elements, which is what keeps the battlefield sweep off
    -- a filter that never names the atom. Order carries nothing -- HasAttached
    -- asks CR 303.4b's existential and nothing indexes the field.
    --
    -- Empty where nothing is attached, and where the candidate is a player (CR
    -- 303.4b's other enchantable): this builder holds a PlayerId and no board
    -- to sweep, so Pawl.Engine.Count.bakePerspective answers HasAttached for a
    -- player candidate instead, ahead of this field ever being read. Narrowed
    -- to attachers on the BATTLEFIELD, as `attachedToView` narrows its host and
    -- for CR 110.1's reason:
    -- an object that has left is no longer a permanent and no longer attached to
    -- anything, the stale window CR 704.5m closes on the next state-based-action
    -- pass.
    --
    -- Recursive, and therefore LAZY, for `attachedToView`'s reason and with its
    -- termination argument unchanged: the elements come from the same `peers`
    -- reader, bounded at the caller's own depth
    -- (Projection.viewOfCharacteristics' peers), so an attacher reached from
    -- inside the layer fold is read through that fold's layers rather than by
    -- re-entering `gather`. Descending into a nest descends the FILTER, which is
    -- finite, so an attachment cycle terminates too.
    attachedViews :: [View],
    -- CR 701.3a / 301.5a: WHICH object this candidate is attached to, for
    -- IsAttachedToSource to compare against Context.source -- the id and not a
    -- Bool, because the atom's answer depends on the match's source and this
    -- record is built once per candidate.
    --
    -- Nothing where Object.attachedTo is, and also where it names a PLAYER (CR
    -- 303.4's other destination), which is Recipient.objectOf's Nothing. Reads no
    -- second projection, so unlike `attachedToView` it needs no laziness
    -- argument -- an ObjectId is not a characteristic. Deliberately NOT narrowed
    -- to a host on the battlefield the way `attachedToView` is; see
    -- Pawl.Engine.Projection.View.viewOfCharacteristics.
    attachedTo :: Maybe ObjectId.ObjectId,
    -- CR 701.3a: could the SUBJECT of the attach now being performed -- the
    -- permanent an Effect.AttachTarget is moving -- legally be attached to this
    -- candidate?
    --
    -- The one field here whose answer depends on something other than the
    -- candidate ALONE, which is why it lives in the per-candidate View rather than
    -- in Context: it needs the subject's enchant ability (CR 702.5a) AND the
    -- candidate's projected characteristics, so it has a different answer per
    -- candidate. Context.sourcePower is the other half of that division -- one
    -- reading of the source, the same for every candidate in the match. Pawl.Engine.Attach.hostsAmong is the only
    -- site that fills it, from Attach.attachmentFor -- the same function that
    -- performs the move, so the offer and the move cannot disagree.
    --
    -- False everywhere else, and that is not a lost distinction: outside an attach
    -- there is no subject for the question to be about. A Filter that named the
    -- atom from any other position would read that vacuous False, so no card is
    -- allowed to -- Pawl.CardSpec rejects it in every Filter position a card has.
    -- No card position is exempt: Effect.AttachTarget's destination is the one
    -- that MAY hold it, and CR 303.4k's is not, because there the enchant-ability
    -- conjunct is the rule's rather than the card's (Attach.turnUpHosts).
    -- The question with the two roles SWAPPED -- a candidate that could be
    -- attached to a fixed host -- is `canAttachToSubject` below rather than a
    -- widening of this field.
    canHostSubject :: Bool,
    -- CR 701.3a read the other way: could THIS CANDIDATE legally be attached to
    -- the object the surrounding instruction fixes -- Auratouched Mage's "an Aura
    -- card that could enchant it", where the host is fixed and the Aura varies?
    --
    -- `canHostSubject` above with the roles swapped, and here for the same reason
    -- it is: the answer needs the candidate's enchant ability (CR 702.5a) AND the
    -- fixed host's projected characteristics, so it differs per candidate.
    -- Pawl.Engine.Resolve's Effect.Search arm is the only site that fills it,
    -- from Pawl.Engine.Attach.attachableWithLastKnown -- whose live half is the
    -- same function that performs the move, and whose other half is CR 608.2h's
    -- reading of a host that has left the battlefield. Which object is fixed is
    -- Pawl.Types.Search.subject, the source or a bound slot.
    --
    -- LAZY, for attachedToView's cost reason: filling it projects the candidate
    -- and sweeps the battlefield for the fixed host's admission, so a search
    -- filter that never names the atom pays nothing.
    --
    -- False everywhere else, and that is not a lost distinction: outside a search
    -- no instruction fixes a host for the question to be about. Pawl.CardSpec
    -- rejects the atom in every Filter position but a search's, the treatment
    -- `canHostSubject` already gets.
    canAttachToSubject :: Bool,
    -- CR 111.1 / 111.6: is this candidate a token rather than a card? Read from
    -- Object.source (Pawl.Engine.Game.isToken), never from a projection -- CR 111.3 makes
    -- a token's effect-defined characteristics equivalent to printed ones, so no
    -- characteristic axis distinguishes the two and no CR 613 layer can change the
    -- answer. False for every candidate with no object behind it.
    token :: Bool,
    -- CR 903.3: is this candidate one of the cards its OWNER designated as a
    -- commander? Read from Pawl.Engine.Commander.isCommander, never from a
    -- projection -- rule 903.3's designation is a printing fixed before the game
    -- begins, so no CR 613 layer can change the answer, `token` above's posture.
    -- False for every candidate with no object behind it.
    commander :: Bool,
    -- CR 113.3b: is this candidate an ACTIVATED ability on the stack, rather
    -- than CR 113.3c's triggered one? Read from Object.source
    -- (Pawl.Engine.Game.isActivatedAbility) exactly as `token` above is, and
    -- never from a projection: which kind of ability an object is is no
    -- characteristic (CR 109.3), so no CR 613 layer can change the answer.
    --
    -- False for every candidate that is not an ability on the stack: a
    -- permanent, a spell, a player, a printed face, an event snapshot. Each
    -- builder says so at its own site.
    activatedAbility :: Bool,
    -- CR 113.1c: is this candidate an ability on the stack AT ALL, of either of
    -- CR 113.3's kinds? The field above widened, filled from the same record
    -- (Pawl.Engine.Game.isAbility) and never from a projection, for the same
    -- reason: which kind of object an object is is no characteristic (CR 109.3).
    --
    -- False for every candidate that is not an ability on the stack, each
    -- builder saying so at its own site.
    ability :: Bool,
    -- CR 114.5: is this candidate an emblem? Read from Object.source
    -- (Pawl.Engine.Game.isEmblem) as the two fields above are, and CR 114.3
    -- makes it more than immutable -- an emblem has no characteristics but its
    -- abilities, so no CR 613 layer has anything here to write.
    --
    -- False for every candidate that is not an emblem. Each builder says so at
    -- its own site.
    emblem :: Bool,
    -- CR 113.7: the view of the object this ability came from, for
    -- Filter.FromSource's nest to be matched against. Filled only by
    -- Pawl.Engine.Projection.View.viewOfCharacteristics, through CR 113.7a's last
    -- known information once the source has left. Nothing for every candidate
    -- that is not an ability on the stack, where the atom is vacuously False.
    abilitySource :: Maybe View,
    -- | CR 110.5a's tap status. Not a characteristic, so no projection writes it;
    -- read straight off the object.
    tapped :: Bool,
    -- | CR 110.5's other status category, and read exactly as `tapped` above is:
    -- status is not a characteristic (CR 110.5a), so no projection writes it and
    -- Pawl.Engine.Projection.View.viewOfCharacteristics reads Object.facing straight
    -- off the object.
    --
    -- Scoped to the BATTLEFIELD there, unlike `tapped` and like `transformed`
    -- below: CR 110.5d gives status to permanents alone, and Pawl.Types.Facing is
    -- deliberately not so scoped -- CR 708.4 has a face-down SPELL carrying the
    -- same value while it waits on the stack. Never Object.exiledFaceDown, which
    -- CR 110.5d says has no correlation to this.
    --
    -- False for every candidate with no permanent behind it: a printed face, a
    -- player, an event snapshot. Each says so at its own site.
    faceDown :: Bool,
    -- | CR 708.12's "the characteristics of that object ignoring any continuous
    -- effects": the view of the CARD REPRESENTING the candidate, built off its
    -- printed face by Pawl.Engine.Projection.View.viewOfCard, for
    -- Filter.RepresentedByCard's nest to be matched against.
    --
    -- Read through Pawl.Engine.Game.faceUpFaceOf rather than Game.faceOf, which
    -- is the whole of what the atom buys: CR 708.2a's substitution lives in
    -- faceOf, so a projected read of a manifested land answers "2/2 creature" and
    -- this one answers "land". The same read CR 701.40b's special action takes at
    -- Pawl.Engine.FaceDown.manifestCostOf.
    --
    -- `Maybe View` and never the candidate's own view, AttachedTo's
    -- `attachedToView` shape: an object with no card behind it -- a token, an
    -- ability on the stack, a player -- is represented by no card at all, and the
    -- atom is vacuously False there rather than falling back on a projection the
    -- rule excludes. Nothing at Pawl.Engine.Projection.View.viewOfCard's own site too,
    -- where the candidate IS a printed face and a nest would recur forever.
    representedCard :: Maybe View,
    -- | CR 406.3's "exiled face down", which the line above is emphatically not:
    -- CR 110.5d says the two have no correlation, so this reads
    -- Object.exiledFaceDown and that one reads Object.facing.
    --
    -- Scoped to no zone, unlike `faceDown` above: Object.exiledFaceDown is
    -- per-incarnation state written only by the move into exile, and CR 400.7
    -- mints a fresh incarnation on the way out, so the field is False everywhere
    -- else without a zone test.
    --
    -- False for every candidate with no object behind it: a printed face, a
    -- player, an event snapshot. Each says so at its own site.
    exiledFaceDown :: Bool,
    -- | CR 701.27g's "transformed permanent": a double-faced permanent on the
    -- battlefield with its back face up. Not a characteristic either (CR
    -- 712.8d/e make which face is up the thing characteristics are read off), so
    -- no projection writes it -- Pawl.Engine.Projection.View.viewOfCharacteristics
    -- reads the CURRENT face off the object and the battlefield off the board.
    --
    -- False for every candidate with no object on the battlefield behind it: a
    -- printed face, a player, an event snapshot. Each says so at its own site,
    -- and none of the three is a lost distinction -- CR 701.27g asks about a
    -- permanent, and none of them is one.
    transformed :: Bool,
    -- | CR 122.1: the counters on the candidate, counted per kind. Not a
    -- characteristic -- CR 109.3's list has no counters in it -- so no projection
    -- writes it, and it deliberately survives ALONGSIDE the power and toughness
    -- CR 613.4c derives from it, because "does it have a +1/+1 counter" and "is
    -- its power 3" are different questions with different answers.
    --
    -- Read by Pawl.Engine.Quantity's ObjectCounters arm. SUPPLIED by the
    -- caller rather than looked up here, the posture `controller` already takes,
    -- which is what lets Pawl.Engine.Projection.viewWithLastKnown hand over CR
    -- 608.2h's record for an object whose id names nothing.
    --
    -- Empty for every candidate with no counters to read: a printed card off the
    -- battlefield, a player, a spell that was cast. An event snapshot of a MOVE
    -- is not one of them -- CR 608.2i looks back at what the object HAD, which
    -- Pawl.Engine.Count.snapshotView answers off the same CR 608.2h record. Its
    -- CARD shape is the exception and reads the object that ARRIVED, whose
    -- counters CR 122.2 has already emptied.
    counters :: Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural.Natural,
    -- | CR 123.1 / 123.4: the kinds of the stickers on the candidate, one per
    -- sticker. Off the object: CR 123.1 keeps stickers out of the copiable values.
    stickerKinds :: Seq.Seq StickerKind.StickerKind,
    -- | CR 123.6: the word on each name sticker on the candidate, in placement
    -- order. Off the object, stickerKinds' posture.
    nameStickers :: Seq.Seq Text.Text,
    -- | CR 123.8a: the printed power and toughness on each P/T sticker on the
    -- candidate, in placement order. Off the object, stickerKinds' posture.
    stickerPowerToughness :: Seq.Seq (Integer, Integer),
    -- CR 701.54a-b: which player this candidate is the Ring-bearer FOR, or Nothing
    -- for the overwhelming majority of permanents, which carry no such
    -- designation. Read straight off Object.ringBearerFor -- CR 701.54b makes it a
    -- designation rather than a characteristic, so no projection writes it, and
    -- the field remembers the player rather than being a bare flag because CR
    -- 701.54a ends the designation when another player gains control (see
    -- Pawl.Engine.Ring.endOnControlChange).
    --
    -- Nothing for every candidate with no object to read it off: a printed card
    -- off the battlefield, a player, an event snapshot -- the vacuous posture
    -- power and controller already take.
    ringBearerFor :: Maybe PlayerId.PlayerId,
    -- CR 702.95b: which creature this candidate is paired with, and under whom.
    -- Read straight off Object.paired, for ringBearerFor's reason -- CR 702.95b
    -- makes pairing a record on the permanent rather than a characteristic, so no
    -- projection writes it.
    --
    -- Nothing for every candidate with no object to read it off: a printed card
    -- off the battlefield, a player, an event snapshot -- ringBearerFor's vacuous
    -- posture, and CR 702.95e leaves nothing paired off the battlefield anyway.
    paired :: Maybe Pairing.Pairing,
    -- Which of Pawl.Types.Designation's marks does this candidate have? Read
    -- straight off Object.designations, for ringBearerFor's reason -- the rules
    -- behind that type make each a designation rather than a characteristic, so no
    -- projection writes it -- and a Set rather than a Maybe because none of them
    -- names a player.
    --
    -- Empty for every candidate with no object to read it off: a printed card off
    -- the battlefield, a player, an event snapshot -- the vacuous posture `tapped`
    -- and `token` already take.
    --
    -- Read by this module's own Filter.HasDesignation arm (Aragorn, Hornburg
    -- Hero's trigger, Rune-Brand Juggler's sacrifice cost) and by
    -- Pawl.Engine.Quantity's HasDesignation arm (renown's intervening "if",
    -- monstrosity's clause condition, Repeat Offender's, and both of Case of the
    -- Ransacked Lab's). What CR 701.60c hangs off `Suspected` does NOT come
    -- through here:
    -- Pawl.Engine.Projection.designationGathered and
    -- Pawl.Engine.CombatRestriction.inForce hold no view and read the object
    -- directly.
    designations :: Set.Set Designation.Designation,
    -- CR 701.37c: what value did each of this candidate's marks get set with?
    -- Read straight off Object.designationValues, and empty where there is no
    -- object to read it off, both for the reasons `designations` above gives. Its
    -- one reader is Pawl.Engine.Quantity's DesignationValue arm, answering Hydra
    -- Broodmaster's token count and the tokens' own printed box.
    --
    -- A number beside the Set rather than inside it, `classLevel` below's reason:
    -- membership and the value are asked by different atoms, and only
    -- "monstrosity X" sets one at all.
    designationValues :: Map.Map Designation.Designation Natural.Natural,
    -- CR 706.8a: the die results stored on this candidate, read straight off
    -- Object.storedResults and empty where there is no object to read it off,
    -- `designationValues` above's route. Its one reader is Pawl.Engine.Quantity's
    -- StoredResultsOfSameValue arm (Centaur of Attention).
    storedResults :: Map.Map StoredResult.StoredResult Natural.Natural,
    -- CR 716.2b: the level designation on this candidate, or Nothing for the
    -- overwhelming majority of permanents, which have never been given one. Read
    -- straight off Object.classLevel, for `designations` above's reason -- rule
    -- 716.2b makes it a designation rather than a characteristic, so no projection
    -- writes it -- and a Maybe rather than a member of that Set because it is a
    -- NUMBER, which is also why it needs its own field rather than a fifth
    -- Pawl.Types.Designation constructor with a `designationValues` entry: CR
    -- 716.2d gives an unlevelled permanent a level, where an unset mark has no
    -- value (see Pawl.Types.Designation).
    --
    -- CR 716.2d's "treated as though its level is 1" is deliberately NOT applied
    -- here: this field reports the mark, and Pawl.Engine.Quantity's ClassLevel arm
    -- defaults at the read, so the one asker the rule speaks to is the one place
    -- that defaults.
    --
    -- Nothing for every candidate with no object to read it off: a printed card
    -- off the battlefield, a player, an event snapshot -- the vacuous posture
    -- `ringBearerFor` above takes.
    classLevel :: Maybe ClassLevel.ClassLevel,
    -- CR 601.2b: how many times was each optional additional cost a keyword of
    -- this candidate offers declared? Read off Object.paidCosts, or off
    -- LastKnown.paidCosts for an object that has left its zone (CR 608.2h), and
    -- empty for a printed card, a player or a cast's snapshot. Its readers are Pawl.Engine.Quantity's WasKicked arm, answering
    -- Burst Lightning's clause conditions and Monstrous War-Leech's CR 604.2
    -- clause on its entry replacement, and its TimesPaid arm, answering Gnarlid
    -- Pack's count, Sunscape Battlemage's "kicked with its {1}{G} kicker" and CR
    -- 702.157a's and CR 702.175a's enters triggers, and Filter.Kicked, answering
    -- Hallar, the Firefletcher's "if that spell was kicked".
    --
    -- Not a designation of a PERMANENT as that field holds -- rule 702.33d
    -- designates the SPELL -- but it comes through the view for the same reason
    -- those do: the reader holds a view and not a board. It is nonetheless filled
    -- for a permanent a kicked spell became, which is CR 400.7d's exception to
    -- the forgetting (see Pawl.Types.Object).
    paidCosts :: Map.Map Keyword.Type.Keyword Natural.Natural,
    -- CR 702.104b: did the opponent rule 702.104a's tribute ability let this
    -- candidate's controller choose have it enter with the +1/+1 counters? Read
    -- off Object.tributePaid, and False where there is no live object -- rule
    -- 702.104a's ability functions only as the creature ENTERS, so the only
    -- reader (Pawl.Engine.Quantity's TributeWasPaid arm, answering Snake of the
    -- Golden Grove's intervening "if") asks it of a permanent that is still there.
    tributePaid :: Bool,
    -- CR 601.2b / 400.7d: the keyword whose candidate cost this candidate was
    -- cast for, read off Object.castUsing, and Nothing where there is no live
    -- object, since Pawl.Types.LastKnown keeps no such record.
    -- Pawl.Engine.Quantity's CastUsing arm is the reader.
    castUsing :: Maybe Keyword.Type.Keyword,
    -- CR 702.143c: is this candidate a foretold card, or a spell that was one
    -- before it was cast? Read off Object.foretold, which Pawl.Engine.Cast
    -- carries onto the stack, and False where there is no live object, as
    -- `castUsing` above. Pawl.Engine.Quantity's WasForetold arm is the reader.
    foretold :: Bool,
    -- CR 400.7d / CR 107.4h: the production tags of the mana that was spent to
    -- cast the spell this candidate is, or was, or to activate the CR 602.2a
    -- ability it is -- read off Object.manaSpent, and empty where there is no
    -- object to read it off, both for the reasons `designations` above gives. Its
    -- readers are Pawl.Engine.Quantity's TagWasSpent arm, answering Berg
    -- Strider's and Forsworn Paladin's clause conditions, and its
    -- TagWasSpentOfOwnColor arm, answering Boreal Outrider's. `manaSpentAmount`
    -- below is the same record's other question.
    --
    -- KEYED by tag, and the value is the colours (CR 202.2) of the units that
    -- carried it: Pawl.Types.ProductionTag is the closed half of what a unit
    -- carries (see its header), so the keys are a classification the vocabulary
    -- can ask about rather than a pool to case over, and the colours are the one
    -- further axis a card conjoins with a tag over ONE unit -- "{S} of any of
    -- that spell's colors". A key is present for every tag spent, with an empty
    -- colour set where every unit carrying it was colourless (CR 106.1b), so
    -- membership is still TagWasSpent's whole question.
    --
    -- Non-empty for a permanent a spell paid with tagged mana became, which is CR
    -- 400.7d's exception to the forgetting (see Pawl.Types.Object.manaSpent).
    manaSpentTagColors :: Map.Map ProductionTag.ProductionTag (Set.Set Color.Color),
    -- | CR 202.1a: how many mana were spent to pay for this candidate -- the
    -- COUNT of the units `manaSpentTagColors` above classifies, read off the same
    -- Object.manaSpent and 0 where there is no object to read it off. Its one
    -- reader is Pawl.Engine.Quantity's ManaSpent arm, answering rule 702.191a's
    -- "the amount of mana spent to cast that spell".
    manaSpentAmount :: Natural.Natural,
    -- CR 602.1 / 605.1a: does the candidate have an activated ability that isn't
    -- a mana ability? A Bool and not the ability list, because that is the whole
    -- of what Filter.HasNonManaActivatedAbility asks and this module holds no
    -- board to measure an ability against.
    --
    -- Filled by the builders that hold an ability list -- Pawl.Engine.Projection's
    -- two and Pawl.Engine.Count.viewOfSnapshot -- which is what keeps CR 605.1a's
    -- test out of here: classifying an ability means importing
    -- Pawl.Engine.ManaAbility, and this module holds no abilities to classify.
    --
    -- LAZY, for attachedToView's cost reason: filling it re-asks CR 702.178a's
    -- grant condition, which reaches a second projection, and `affects` builds a
    -- view from inside a projection already. Nothing forces it unless a Filter
    -- actually contains the atom: Tsabo's Web reads it outside the layer fold,
    -- while Synthetic Artificers' Ascent, Synthetic Ability Audit and Synthetic
    -- Tinker's Conversion read it from inside one, which the bounded reader
    -- below is what makes safe.
    --
    -- Bounded like attachedToView, and for the same reason: the condition is
    -- asked through the reader Pawl.Engine.Projection.View.viewOfCharacteristics was
    -- handed, so a filter forcing this from inside the fold reads the board at
    -- that caller's own layer rather than restarting the projection (CR 613.1).
    nonManaActivatedAbility :: Bool,
    -- CR 602.1: does this candidate have one or more activated abilities at all?
    -- The field beside it without CR 605.1a's exclusion, filled by the same
    -- builders and LAZY and bounded for the same two reasons. Named apart from
    -- `activatedAbility` above, which asks whether the candidate IS one.
    --
    -- WIDER than the abilities that field measures, and by exactly CR 305.6's
    -- intrinsic "{T}: Add [mana symbol]", which no ability list holds: every
    -- builder answers it through Pawl.Engine.Subtype's intrinsicManaAbility --
    -- intrinsicManaAbilityOf where there is a projection to read CR 613.1f's
    -- layer-6 removal off -- and
    -- Pawl.ProjectionSpec's "CR 305.6 / 602.1 a Mountain has an activated ability
    -- in every view builder" is what holds the three to one answer. The field
    -- above needs no such disjunct, CR 605.1a excluding a mana ability from it.
    hasActivatedAbility :: Bool,
    -- CR 702.184c: does this candidate's controller's station abilities read a
    -- tapped creature's toughness instead of its power? The projected mirror
    -- of ProjectedCharacteristics.grantsStationToughness -- read off the
    -- projection on the battlefield and off the printed face otherwise (a
    -- static ability functions in every zone CR 113.6 allows, and Tapestry
    -- Warden's own is unrestricted). No Filter atom consults it: it is here
    -- for Pawl.Engine.Quantity's StationMeasure arm, which walks the
    -- battlefield through the same ViewOf every other Quantity arm reads.
    --
    -- False on a player view: CR 109.1 gives a player no controller and so no
    -- permanent whose Modification this could be.
    grantsStationToughness :: Bool
  }

-- The view of a PLAYER candidate: no card types, no colours, no controller --
-- a player is not an object (CR 109.1) and has none of those. The player's own
-- identity is the only thing THIS VIEW can answer, which is exactly what
-- IsPlayer asks. The other player-shaped atoms are answered elsewhere on
-- purpose: ControlsMoreThanYou, IsControllerOfBound and CardsInGraveyardAtLeast
-- each ask about the board or a zone, which a function taking nothing but a
-- PlayerId cannot see, so Pawl.Engine.Count.bakePerspective bakes them against
-- the game state before the match instead.
--
-- CR 120.1's "was dealt damage this turn" is the one field a caller FILLS rather
-- than bakes, since it is a characteristic of the candidate rather than a
-- comparison; Pawl.Engine.Count.playerView is that caller, and the field says so.
playerView :: PlayerId.PlayerId -> View
playerView pid =
  MkView
    { -- CR 201.1 gives a name to an OBJECT, and CR 109.1's list of what an
      -- object is has no player in it.
      names = Set.empty,
      cardTypes = Set.empty,
      supertypes = Set.empty,
      colors = Set.empty,
      subtypes = Set.empty,
      -- CR 702.1: a keyword ability is an ability OF AN OBJECT, and CR 109.1's
      -- list of what an object is has no player in it.
      keywords = Set.empty,
      power = Nothing,
      toughness = Nothing,
      intensity = Nothing,
      -- CR 202.3 reads a mana cost, which is printed on an OBJECT (CR 202.1); a
      -- player has none.
      manaValue = Nothing,
      -- CR 202.1 prints a mana cost on a CARD; a player has none, as they have
      -- no mana value above.
      manaCost = Nothing,
      controller = Nothing,
      -- CR 108.3 gives an owner to a CARD; a player owns cards and is not one.
      owner = Nothing,
      -- CR 903.3 designates a CARD; a player designates one and is not one.
      commander = False,
      -- CR 400.1 puts OBJECTS in zones; a player is in none of them (CR 109.1).
      zone = Nothing,
      -- CR 601.2a casts an OBJECT; a player was never cast either.
      castFrom = Nothing,
      -- CR 115.1: a player is never on the stack, so targets nothing.
      targets = Set.empty,
      targetViews = Map.empty,
      targetCount = 0,
      identity = Nothing,
      playerIdentity = Just pid,
      -- CR 506.3: only a creature can attack, and a player is not one.
      attacking = False,
      -- CR 506.3 again: a player attacks nothing, so there is no player it is
      -- attacking either.
      attackingPlayer = Nothing,
      -- CR 506.3 a third time: a player attacks no planeswalker either.
      attackingPlaneswalkerController = Nothing,
      -- CR 506.3 a fourth time: nor any battle.
      attackingBattleProtector = Nothing,
      -- CR 508.3b lets a PLAYER be declared attacked, so unlike the field above
      -- this one asks a question a player candidate really can answer -- just not
      -- from here, this view being built from a PlayerId alone and holding no
      -- combat record to read. False is therefore the DEFAULT and not the answer,
      -- exactly as `dealtDamageThisTurn` below is: Pawl.Engine.Count.playerView
      -- overwrites it from GameState.combat.
      declaredAttackedThisCombat = False,
      -- CR 509.1a: only a creature can block, either.
      blocking = False,
      -- CR 509.1h: blocked-ness is a status of an ATTACKING creature, and by CR
      -- 506.3 a player never is one.
      blocked = False,
      blockers = Set.empty,
      -- CR 506.3 again: a player was never declared as an attacker either.
      attackedThisTurn = False,
      -- CR 506.3 once more, for the two combat-phase-scoped questions: only a
      -- creature is ever declared as an attacker or a blocker.
      declaredAttackerThisCombat = False,
      declaredBlockerThisCombat = False,
      -- CR 701.17a mills CARDS, and a player is not one.
      milledThisTurn = False,
      -- CR 120.1 lets a player BE dealt damage, so unlike the two fields above
      -- this one asks a question a player candidate really can answer -- just not
      -- from here, this view being built from a PlayerId alone and holding no
      -- board whose event log could be folded. False is therefore the DEFAULT and
      -- not the answer: Pawl.Engine.Count.playerView overwrites it from
      -- Game.wasDealtDamageThisTurn, and both roads a player candidate travels --
      -- that module's Scope.OverPlayers fold and
      -- Pawl.Engine.Target.admittedGiven's Recipient.ToPlayer arm -- build the
      -- view through it. Pawl.DamageSpec's Needle Drop case is what proves it.
      dealtDamageThisTurn = False,
      -- CR 110.1: only a permanent is on the battlefield, and a player is not one.
      enteredThisTurn = False,
      -- CR 702.122b crews with a CREATURE, and a player is not one -- CR 506.3
      -- rules the combat fields above out for the same kind of reason.
      crewedThisTurn = Set.empty,
      convokedThisTurn = Set.empty,
      saddledThisTurn = Set.empty,
      -- CR 302.6's continuity is about a creature a player CONTROLS, and a player
      -- is not one -- False is the answer here rather than a default.
      controlledSinceTurnBegan = False,
      -- CR 303.4b: a player an Aura is attached to is ENCHANTED by it; the
      -- player is not itself attached to anything, because Object.attachedTo is
      -- a field of the ATTACHED permanent, and a player is not one. So there is
      -- no host to evaluate AttachedTo's nest against, and the atom is vacuously
      -- False for a player candidate however the nest is written.
      attachedToView = Nothing,
      -- CR 303.4b lets an Aura enchant a PLAYER, so unlike the field above this
      -- one asks a question a player candidate really can answer -- but not here:
      -- this view is built from a PlayerId alone and holds no board to sweep for
      -- the attachers. False is therefore the DEFAULT and not the answer, but
      -- unlike `dealtDamageThisTurn` above it stays the default here: CR 109.3
      -- keeps attachment off the characteristics, so this is ControlsMoreThanYou's
      -- posture rather than that field's -- Pawl.Engine.Count.bakePerspective
      -- rewrites the HasAttached atom against the board instead of filling this
      -- field, and only the Scope.OverPlayers road travels through it.
      attachedViews = [],
      -- CR 303.4 again: a player is attached to nothing, so there is no host id
      -- for IsAttachedToSource to compare either.
      attachedTo = Nothing,
      -- CR 701.3a's question can be asked about a player (CR 702.5d), but not
      -- here: the only site that fills this field is Pawl.Engine.Resolve's
      -- AttachTarget arm, whose candidates are battlefield permanents.
      canHostSubject = False,
      -- CR 303.4b again: an Aura attached to a player enchants them, so the
      -- question CAN be asked of a player candidate (CR 702.5d) -- but not here,
      -- since the only site that fills this field is Pawl.Engine.Resolve's
      -- Effect.Search arm, whose candidates are cards in a library, a graveyard
      -- or a hand.
      canAttachToSubject = False,
      -- CR 111.1: a token represents a PERMANENT, and a player is not one.
      token = False,
      activatedAbility = False,
      -- CR 113.1c again, one kind wider: a player is not an object at all (CR
      -- 109.1), so it is no ability on the stack either.
      ability = False,
      -- CR 114.5 / 109.1: a player is not an object, and so not an emblem.
      emblem = False,
      -- CR 113.7 asks about an ability on the stack, and CR 109.1 makes a player
      -- none.
      abilitySource = Nothing,
      tapped = False,
      -- CR 110.5d gives status to PERMANENTS, and CR 109.1 makes a player none of
      -- those either -- the line below's reason, one status category over.
      faceDown = False,
      -- CR 708.12 asks about the card representing an OBJECT, and CR 109.1 makes a
      -- player none -- `faceDown` above's reason, one rule over.
      representedCard = Nothing,
      -- CR 406.2's exiled card is a CARD, and CR 109.1 makes a player none of
      -- those either.
      exiledFaceDown = False,
      -- CR 701.27g asks about a PERMANENT, and CR 109.1 makes a player none.
      transformed = False,
      -- CR 122.1 puts counters on an object OR a player, and a player's are
      -- Player.counters, read by Quantity.PlayerCounters. This field is the
      -- OBJECT half, so a player view has none of it.
      counters = Map.empty,
      -- CR 123.1: a sticker is on an object, and CR 109.1 makes a player none.
      stickerKinds = Seq.empty,
      nameStickers = Seq.empty,
      stickerPowerToughness = Seq.empty,
      -- CR 701.54b: Ring-bearer is a designation A PERMANENT can have, and a
      -- player is not one -- the same shape CR 725.1's monarch has with the two
      -- sides swapped.
      ringBearerFor = Nothing,
      paired = Nothing,
      -- CR 702.112b: "only permanents can be or become renowned", CR 701.37b,
      -- CR 701.60b and CR 719.3b saying the same of the other marks, and a
      -- player is not one.
      designations = Set.empty,
      -- CR 701.37c's X belongs to a mark a player cannot have -- `designations`
      -- above, same sentence.
      designationValues = Map.empty,
      storedResults = Map.empty,
      -- CR 716.2b: a level is a designation A PERMANENT can have, and a player is
      -- not one -- `designations` above, same sentence.
      classLevel = Nothing,
      paidCosts = Map.empty,
      -- CR 702.104a's tribute is a creature's static ability, and a player is not
      -- one -- `designations` above, same sentence.
      tributePaid = False,
      castUsing = Nothing,
      foretold = False,
      -- CR 202.1a's mana cost is spent to cast a CARD, and CR 109.1's list of
      -- what an object is has no player in it -- `manaValue` above, same rule.
      manaSpentTagColors = Map.empty,
      -- A player is no object to have been paid for -- `manaSpentTagColors` above,
      -- same sentence.
      manaSpentAmount = 0,
      -- CR 602.1: an activated ability is an ability OF AN OBJECT, and CR 109.1's
      -- list of what an object is has no player in it -- `keywords` above, one
      -- rule over.
      nonManaActivatedAbility = False,
      -- CR 602.1 again, one exclusion out: a player has no abilities either.
      hasActivatedAbility = False,
      -- CR 702.184c reaches a permanent's controller, and a player view built
      -- from a bare PlayerId has no permanent behind it to grant this at all.
      grantsStationToughness = False
    }

-- The perspective the match is relative to: who counts as "you" (CR 109.5), and
-- which object the surrounding effect comes from. Both are Nothing when no
-- player and no source frame the match (an off-battlefield search).
data Context = MkContext
  { -- | CR 808.1: which team each player is on, so that the relation atoms below
    -- can take CR 102.3's teammates out of a candidate's opponents. Supplied by
    -- the caller for `defendingPlayer`'s reason: this module holds no game state,
    -- and every caller in the engine hands it the board's own
    -- (Pawl.Engine.Game.teams). Teams.none is CR 102.4's game that is not played
    -- between teams, which is what a match with no board behind it gets.
    teams :: Teams.Teams,
    perspective :: Maybe PlayerId.PlayerId,
    source :: Maybe ObjectId.ObjectId,
    -- CR 208.1: the SOURCE's power, for the Measures atom's OfSource operand
    -- (CR 702.134a's mentor, CR 702.149a's training). Not derivable from
    -- `source` here -- this module holds no game state and cannot project -- so
    -- Pawl.Engine.Projection.withCharacteristicsOf fills it and the four fields
    -- below together, through last known information (CR 608.2b's re-check,
    -- CR 608.2h's effects): in every context Pawl.Engine.Projection.sourceContext
    -- frames, and in CR 509.1b's pairwise restrictions, which frame by the
    -- creature being compared (Projection.pairwiseContext, Spitfire Handler).
    --
    -- LAZY, and load-bearingly so: filling it costs a projection of the source,
    -- and no filter that omits the operand ever forces it.
    --
    -- Nothing in `contextFor` below and inside the CR 613 layer fold, whose
    -- contexts (Pawl.Engine.SourceContext) cannot project their own source; the
    -- operand then matches nothing, and Pawl.FilterPositionLintSpec keeps a card
    -- out of those positions.
    sourcePower :: Maybe Integer,
    -- CR 208.1: the SOURCE's toughness, for the same operand (Ironclaw Curse).
    -- Filled with sourcePower.
    sourceToughness :: Maybe Integer,
    -- CR 202.3: the SOURCE's mana value, for the same operand (CR 702.85a's
    -- cascade, CR 702.53a's transmute, CR 702.71a's transfigure, Kami of
    -- Mourning). Filled with sourcePower.
    sourceManaValue :: Maybe Integer,
    -- CR 105.2: the SOURCE's colours, for SharesColorWithSource (CR 702.78a's
    -- conspire). Filled with sourcePower. A SET rather than a Maybe: empty is
    -- the right answer both where it is unfilled and for a colourless source.
    sourceColors :: Set.Set Color.Color,
    -- CR 201.1 / 709.4a: the NAMES of the SOURCE, for SameNameAsSource (CR
    -- 702.60a's ripple); `slotNames` below asks the same of a SLOT. Filled with
    -- sourcePower, and a SET for sourceColors' reason (CR 708.2a's nameless
    -- object).
    sourceNames :: Set.Set CardName.CardName,
    -- CR 202.3, the computed half: the number the TARGET SLOT being matched
    -- names as its bound, for the Measures atom's EnclosingAmount operand --
    -- Celestine, the Living Saint's "where X is the amount of life you gained
    -- this turn". The slot carries the Quantity (Pawl.Types.TargetSlot's
    -- `amount`); this is that Quantity already evaluated, because this module
    -- holds no game state and cannot evaluate one.
    --
    -- Pawl.Engine.Target.slotContext fills it for a target slot, and
    -- Pawl.Engine.Projection.View.referenceAdmits for a conjure's reference pick
    -- (Pawl.Types.FromReference's `amount`). It is Nothing at every other
    -- position, and Pawl.FilterPositionLintSpec's lint is what keeps a card out of
    -- them.
    --
    -- LAZY like sourcePower, and load-bearingly so: filling it costs a whole
    -- Quantity evaluation, and no filter that omits the atom ever forces it.
    slotAmount :: Maybe Integer,
    -- CR 601.2b: the slot NAMES a computed bound and the announcement that fixes
    -- it has not been made yet, so `slotAmount` above is Nothing for a reason that
    -- is not "no bound was stated". None of the three atoms that read it
    -- narrows then --
    -- Stir the Grave's "mana value X or less" states no ceiling until its caster
    -- names X, and CR 601.2b puts no ceiling on the value they may name.
    --
    -- The bound's counterpart of the X=0 FLOOR every castability gate asks a
    -- slot's COUNT at (Pawl.Engine.Target.fillableModesGiven): permissive is the
    -- opposite direction for a bound, so the two floors are spelled differently
    -- and mean the same thing -- CR 700.2a's mode is refused only for what the
    -- announcement cannot change.
    --
    -- False in contextFor below, and False at CR 601.2c
    -- and CR 608.2b alike: by then the announcement holds the number, and a bound
    -- that still cannot be read is vacuously False -- the refusing direction, as
    -- slotNames' atom takes and slotControllers' atom does not.
    -- Pawl.Engine.Target.slotContext is the ONE
    -- site that can set it True.
    boundUnannounced :: Bool,
    -- CR 508.5: the DEFENDING PLAYER for the source, for the one atom that asks
    -- (ControlledByDefendingPlayer, CR 702.39a). One player for an attacking
    -- source; for a source with no attack to resolve it, CR 802.2a's every
    -- defending player the controller could choose (Yare). Supplied by the caller for
    -- sourcePower's reason -- this module holds no game state and cannot read the
    -- combat record -- by Pawl.Engine.Target.admittedGiven for a target slot and
    -- by Pawl.Engine.CombatRestriction.inForce for a CR 508.1c gate.
    --
    -- LAZY like sourcePower, and load-bearingly so: filling it costs a
    -- control-grant walk, and no filter that omits the atom ever forces it.
    defendingPlayers :: [PlayerId.PlayerId],
    -- The player the surrounding effect is CURRENTLY BEING APPLIED TO, for the
    -- two atoms that ask (ControlledByRecipient, OwnedByRecipient) --
    -- Biorhythm's "the number of creatures they control", Stronghold Discipline's "1 life for each creature
    -- they control". Supplied by the caller for defendingPlayer's reason, and by
    -- two callers: Pawl.Engine.Resolve's evaluateForRecipient, which every
    -- per-player opcode evaluates its amount through, once per recipient with this
    -- field pointed at each in turn; and Pawl.Engine.PlayerEffect.printedRows,
    -- which asks a player static ability's condition once per affected player
    -- (Ethersworn Canonist's "each player who has cast a nonartifact spell").
    --
    -- NOT `perspective` re-pointed, which would be the cheap version of the same
    -- thing and a wrong one: CR 109.5's "you" is the resolving spell's controller
    -- for the whole quantity, so a card reading both "they control" and "you
    -- control" in one sentence needs the two to disagree.
    --
    -- Nothing wherever the atom cannot appear, which is everywhere else.
    recipient :: Maybe PlayerId.PlayerId,
    -- The objects the surrounding announcement's or resolution's slots name,
    -- for Quantity.AgainstSlot to aim an evaluation at one, for Count's
    -- OverBound fold and for the IsBound atom above. It rides here because
    -- this record is already the evaluation context every Quantity is handed,
    -- and a slot map is exactly the part of a resolution the evaluator cannot
    -- derive.
    --
    -- A SET per slot, because a slot may name several objects at once: CR
    -- 601.2c's target slot counted past one (Command the Dreadhorde's "those
    -- cards") and CR 115.10a's group binding (Act on Impulse's "those cards",
    -- Midnight Tilling's "from among them"). The readers that can take no more
    -- than one ask through `slotOneObject` below rather than off this map
    -- directly. An EMPTY set never appears -- a slot naming nothing is an absent
    -- key -- so `Map.member` and "names something" are the same question.
    --
    -- Pawl.Engine.Binding.objectsBySlot decides what each slot names, and
    -- Pawl.Engine.Projection.framedBySlots fills this field with every field
    -- derived from it. What differs between producers is only WHICH slot map,
    -- and each is the one its rule names: a target slot's filter
    -- (Pawl.Engine.Target) reads the announcement (CR 601.2c); a resolution's
    -- positions (Pawl.Engine.Resolve.Slots.effectContext) the targets CR 608.2b
    -- left legal; CR 603.4's two intervening-"if" checks the trigger's own
    -- bindings, before CR 608.2b; Pawl.Engine.Replacement.candidateContext the
    -- snapshot ActiveReplacement.slots holds, the resolution that installed the
    -- row being over; Pawl.Engine.Cost's candidate pools the announced stack
    -- object's bindings (Cost.announcedSlots), CR 601.2c choosing the targets
    -- before CR 601.2h pays; Pawl.Engine.Event.Match.matchesTriggerGiven a
    -- delayed ability's captured environment (CR 603.7c); and
    -- Pawl.Engine.Mana's priced count a payment's slots.
    --
    -- Outside those, contextFor leaves it empty, and every atom that reads it
    -- (IsBound, SameNameAsBound, IsControllerOfBound, Quantity.AgainstSlot) is then vacuously False or Nothing rather than
    -- raising. That is honest wherever no announcement is in flight -- the layer
    -- fold, matching a trigger that captured nothing, a cost paid with nothing announced, combat
    -- declarations, duration expiry -- and it was not honest of every
    -- in-resolution caller until each took its context from the caller instead:
    -- Pawl.Engine.Projection.freezeQuantities first, then Resolve's
    -- Effect.Search arm, its Effect.Mill tally, and Pawl.Engine.Attach.hostsFor
    -- last, which takes a Context in place of a controller and a source, so its
    -- two resolution callers hand over effectContext and its two standing
    -- callers a bare contextFor.
    --
    -- Pawl.Engine.Event.eligible is a further in-resolution caller and is
    -- honest for a reason of its own rather than for the
    -- reason above: its candidates are cards outside the game, which CR 400.11c
    -- keeps every spell and ability from affecting, so no slot of the resolution
    -- can name one. Pawl.Engine.Projection.View.viewOfCard fills no `identity` --
    -- nothing mints an object until Pawl.Engine.Event.bringInto does --
    -- so IsBound is False for every candidate there whatever this map holds. The
    -- other readers cannot reach it either: SameNameAsBound reads slotNames
    -- rather than this map and carries its own lint, SameControllerAsBound reads
    -- slotControllers and carries one that matters MORE, its vacuous direction
    -- being True, SameControllerAsHostOfBound reads slotHostControllers and
    -- refuses on an absent key, IsControllerOfBound is False wherever `matches`
    -- reaches it, ControlledByBound is False for a card with no controller (CR
    -- 108.4), and no Filter atom carries a Quantity.
    -- Pawl.CardSpec's "CR 400.11c no card asks IsBound
    -- in a wish's filter" is what keeps a card out of that position, and
    -- Pawl.OutsideTheGameSpec proves the atom answers nothing there.
    slotObjects :: Map.Map SlotName.SlotName (Set.Set ObjectId.ObjectId),
    -- CR 702.122d / 101.2: the objects an effect in force says CAN'T CREW
    -- VEHICLES, for the one atom that asks (CantCrewVehicles). Supplied by the
    -- caller for sourcePower's reason -- this module holds no game state and
    -- cannot gather another permanent's static abilities -- and by ONE caller,
    -- Pawl.Engine.Cost.tapCandidates, which is the single pool both CR 118.3's
    -- payability gate and rule 702.122a's payment prompt read.
    --
    -- LAZY, and load-bearingly so: filling it walks the battlefield through
    -- Pawl.Engine.CrewRestriction.cantCrew, and no filter that omits the atom
    -- ever forces it -- which is what lets that one caller supply it for every
    -- tapping cost while only a crew ability's criterion pays for it.
    --
    -- EMPTY in contextFor below and so in every other builder, leaving the atom vacuously False and `Not CantCrewVehicles`
    -- vacuously True: a position with no prohibition gathered admits every
    -- candidate, which is the direction a prohibition nobody printed must take.
    cantCrewVehicles :: Set.Set ObjectId.ObjectId,
    -- CR 201.1 / 709.4a: the NAMES of the objects the surrounding announcement's
    -- slots hold, for the one atom that compares a candidate's against them
    -- (SameNameAsBound, Harness the Storm). Supplied by the caller for
    -- sourcePower's reason -- this module holds no game state and cannot read an
    -- object's names -- through Pawl.Engine.Projection.framedBySlots, where a
    -- target slot's Filter is matched and where every one of a resolution's
    -- positions is (Bifurcate's search filter, Hour of Glory's hand sweep, Grim
    -- Reminder's life loss amount, an attach destination).
    --
    -- Separate from `slotObjects` above rather than derived from it, and that is
    -- the same division sourcePower makes against `source`: an id is not a name
    -- until a board has been asked.
    --
    -- LAZY, and load-bearingly so: filling it costs one projection per bound
    -- object, and no filter that omits the atom ever forces it.
    --
    -- EMPTY in contextFor below, so the atom is vacuously False in a position
    -- framedBySlots does not frame. Which
    -- direction an unfilled read takes is the ATOM's choice rather than a rule
    -- this record imposes: the arm in `matches` below decides it, and
    -- slotControllers' SameControllerAsBound chooses True where this one chooses
    -- False.
    --
    -- What keeps a card out of those positions is Pawl.FilterPositionLintSpec's
    -- "CR 709.4a no card asks SameNameAsBound outside a mode's target slot, a search filter, a hand sweep or a life loss amount",
    -- the sweep sourcePower's and defendingPlayer's siblings each have.
    slotNames :: Map.Map SlotName.SlotName (Set.Set CardName.CardName),
    -- CR 110.2: the CONTROLLERS of the objects the surrounding announcement's
    -- slots hold, for the one atom that compares a candidate's against them
    -- (SameControllerAsBound, Bioshift). `slotNames` above in every respect --
    -- filled by Pawl.Engine.Projection.framedBySlots for a target slot's Filter
    -- and a resolution's positions (Glamer Spinners' attach destination),
    -- separate from `slotObjects` because an id
    -- is not a controller until a board has been asked, lazy so that a filter
    -- omitting the atom never forces the projection, and read through CR 608.2h's
    -- last-known reader so a bound object that has left is still answerable.
    --
    -- What keeps a card out of the positions this is empty in is Pawl.CardSpec's
    -- "CR 110.2 no card asks SameControllerAsBound outside a mode's target slot or an attach destination",
    -- slotNames' sweep with more riding on it: that atom is a silent False
    -- elsewhere and this one a silent True.
    --
    -- ABSENT versus EMPTY is the one place it differs, and the atom's vacuous
    -- direction is why: a key is here for every slot the caller bound, so a
    -- missing key is a slot nothing has named yet -- CR 601.2c's offer, made
    -- before either target is chosen -- and an empty set is a bound object with no
    -- controller at all (CR 108.4's card in a library). The atom widens on the
    -- first and refuses on the second.
    slotControllers :: Map.Map SlotName.SlotName (Set.Set PlayerId.PlayerId),
    -- CR 110.2 with CR 303.4b: the controllers of the permanents the objects the
    -- resolution's slots hold are ATTACHED TO, for the one atom that compares a
    -- candidate's controller against them (SameControllerAsHostOfBound, Simic
    -- Guildmage's "another permanent with the same controller"). One question
    -- further along than `slotControllers` above -- that field asks who controls
    -- the bound object, this one who controls what the bound object enchants --
    -- and a separate field rather than a read through `slotObjects` for that
    -- field's reason: an id is neither a host nor a controller until a board has
    -- been asked.
    --
    -- `slotCreatureTypes` below in every other respect: lazy, filled by
    -- Pawl.Engine.Projection.framedBySlots, empty elsewhere where the atom is a
    -- silent False, and Pawl.FilterPositionLintSpec's "CR 110.2 no card asks
    -- SameControllerAsHostOfBound outside a resolution's own positions" to keep
    -- a card to the position the pool exercises.
    --
    -- Read LIVE rather than through CR 608.2h's last-known reader, which is the
    -- one place it parts from its neighbours: the bound object's HOST is a
    -- battlefield permanent, and "the permanent the Aura is attached to" is a
    -- question about the board as the ability resolves. CR 608.2b has already
    -- dropped a target Aura that left.
    slotHostControllers :: Map.Map SlotName.SlotName (Set.Set PlayerId.PlayerId),
    -- CR 205.2a with CR 303.4b: the CARD TYPES of the permanent the SUBJECT of
    -- the attach now being performed is attached to, for the one atom that asks
    -- about them (HostOfSubjectHasCardType, Enchantment Alteration's "another
    -- permanent of that type").
    --
    -- Here rather than in the per-candidate View, where `canHostSubject` is, for
    -- the division that field's own note draws: this is one reading of the
    -- subject's host, the same for every candidate in the match, which is
    -- `sourcePower`'s side of it. Keyed by nothing, because the subject is not a
    -- slot the caller bound but the permanent Pawl.Engine.Attach.hostsAmong was
    -- handed -- so hostsAmong fills it into the Context it took from its caller
    -- rather than the caller filling it, and hostsAmong is the one filler.
    --
    -- EMPTY everywhere else, where the atom is a silent False, and
    -- Pawl.FilterPositionLintSpec's "CR 205.2a no card asks
    -- HostOfSubjectHasCardType outside a position an attach frames" keeps a card
    -- out of those. Empty is also an honest answer inside an attach: a subject
    -- attached to nothing, or to a player (CR 303.4b), has no host card type.
    --
    -- Read through the PROJECTION (CR 613), not the printed face: a creature
    -- animated into a land, and a land Song of the Dryads has turned into one,
    -- are the type the rule asks about.
    subjectHostCardTypes :: Set.Set CardType.CardType,
    -- CR 205.3m: the CREATURE TYPES of the objects the slots hold, for
    -- SharesCreatureTypeWithBound -- Heirloom Blade's "shares a creature type
    -- with it", through Binding.triggerSource a kinship card's "with this
    -- creature", and a sibling target slot's (Unbury). Filled by
    -- Pawl.Engine.Projection.framedBySlots; empty elsewhere, where the atom
    -- widens, and
    -- Pawl.FilterPositionLintSpec's "CR 205.3m no card asks
    -- SharesCreatureTypeWithBound outside a resolution's own positions or a
    -- target slot" keeps a card out of those.
    slotCreatureTypes :: Map.Map SlotName.SlotName (Set.Set Subtype.Subtype),
    -- CR 608.2c: the MEASURES of the object a resolution's slot holds, for the
    -- Measures atom's OfBound operand (Profaner of the Dead's "the exploited
    -- creature's toughness"). `slotCreatureTypes` above in every respect but one
    -- -- the same filler (Pawl.Engine.Projection.framedBySlots), the same CR
    -- 608.2h last-known reader so the sacrificed creature is still answerable, the
    -- same laziness, the same vacuous False elsewhere, and
    -- Pawl.FilterPositionLintSpec's "CR 208.1 no card compares against a bound
    -- object outside a resolution's own positions" to keep a card to the position
    -- the pool exercises.
    --
    -- ONE number per slot and measure rather than a set, and a slot naming
    -- several objects has no key at all: CR 115.10a's group binding is read by
    -- "those cards" payloads, and no printed comparison asks a group for a
    -- single number.
    slotMeasures :: Map.Map (SlotName.SlotName, Measure.Measure) Integer,
    -- CR 601.2c / 603.2: the PLAYERS the surrounding resolution's slots name --
    -- `slotObjects` above's player half (Pawl.Engine.Binding.playersBySlot),
    -- filled beside it by Pawl.Engine.Projection.framedBySlots.
    --
    -- A replacement's CONDITION leaves it empty: Pawl.Types.ActiveReplacement
    -- captures only the object half of the installing resolution's slots. Its
    -- PATTERN names a slot's player through ControllerRelation.InSlot instead,
    -- baked as the row is installed. Scryfall `o:/(this turn|until end of turn),
    -- if (target|that) (player|opponent)/`, 2026-10-03, answers Plagiarize alone,
    -- whose "if" is its pattern; a floating row whose condition named a slot's
    -- player would need the half.
    --
    -- NO atom in `matches` below reads it. It is a channel THROUGH this record to
    -- Pawl.Engine.Count.playersFor, which is handed CR 113.7's SOURCE and needs
    -- the resolution's own slots instead: an ability's targets and its trigger's
    -- bindings are stamped on the ABILITY object on the stack, and a source is not
    -- its stack object, so a read off the source's bindings answers nothing for
    -- every ability (see #1783). A spell is the one object for which the two agree.
    --
    -- ABSENT versus EMPTY as slotControllers above, and the two mean what they do
    -- there: a key is here for every slot the caller bound, so a missing key is a
    -- position that bound no such slot -- where playersFor falls back to the
    -- source's own bindings, the read every caller building no resolution context
    -- still gets -- and an empty set is a slot bound to no player at all, which
    -- that function declines rather than widening.
    slotPlayers :: Map.Map SlotName.SlotName (Set.Set PlayerId.PlayerId),
    -- CR 601.2b / 603.2: the NUMBERS the surrounding announcement's bindings hold,
    -- keyed by slot -- "that much" as the trigger's own event stamped it
    -- (Pawl.Engine.Binding.eventAmount), the X a caster just named
    -- (Pawl.Engine.Binding.variableX), and the amount an effect of the resolution
    -- itself stamped on a slot (Pawl.Engine.Resolve.Effect.bindAmountSlot).
    --
    -- TWO readers. It is a channel THROUGH the context to Pawl.Engine.Quantity's
    -- InSlot arm, which Pawl.Engine.Target.slotContext evaluates a target slot's
    -- CR 202.3 computed bound in, and which has no other way to reach an
    -- announcement not yet stamped on an object: CR 603.3d chooses a trigger's
    -- targets before the ability object carries any binding at all, and CR 601.2c
    -- chooses a spell's before CR 601.2i stamps the X onto it. It is also what the
    -- Measures atom's AmountInSlot operand reads, which is a read `matches` makes
    -- directly.
    --
    -- The other fillers: Pawl.Engine.Resolve.Slots.effectContext supplies the
    -- resolving object's own stamped amounts, which is the position that operand
    -- is written in, and CR 603.4's two intervening-"if" checks
    -- (Pawl.Engine.Event.Trigger.interveningHolds and CR 608.2a's re-check,
    -- Pawl.Engine.Stack.interveningStillHolds) supply CR 107.3m's announced X
    -- through Pawl.Engine.Condition.inheritedX -- an enters-the-battlefield
    -- trigger's alone, and off the entering PERMANENT, which CR 400.7 left with no
    -- bindings to stamp it on.
    --
    -- Empty in contextFor below, so a bound evaluated
    -- outside a target slot and outside a resolution reads no announcement. What that unfilled read
    -- then ANSWERS is Pawl.Engine.Quantity's InSlot arm's call, not this record's:
    -- each reader of each field here picks its own vacuous direction, and
    -- slotControllers above is the one that picks the widening one.
    boundAmounts :: Map.Map SlotName.SlotName Natural.Natural,
    -- | CR 123.6e's "that sticker": the sticker an earlier instruction bound at
    -- each slot. Empty in contextFor; filled by Resolve.Slots.effectContext and
    -- Target.slotContext.
    slotStickers :: Map.Map SlotName.SlotName StickerRef.StickerRef,
    -- CR 303.4b's "enchanted": WHICH object the SOURCE is attached to, for the one
    -- atom that compares a candidate against it (IsHostOfSource). The id and not a
    -- view, because the answer is one reading of the source and the same for every
    -- candidate -- the division sourcePower makes against `source`. View.attachedTo
    -- is the same read in the other direction, per candidate.
    --
    -- Filled, live off the board (Pawl.Engine.Game.hostOf), by
    -- Pawl.Engine.SourceContext with every other source-derived field, so every
    -- position framed by its source answers it (Ray of Frost, Oppressive Rays,
    -- Pariah).
    --
    -- Nothing in contextFor below, and the atom answers False on that Nothing --
    -- its own arm's choice, not a rule about this record, slotControllers above
    -- being the field whose atom chooses True instead. What keeps a card out of
    -- the positions read through a bare contextFor is Pawl.FilterPositionLintSpec's
    -- "CR 303.4b no card asks IsHostOfSource where the source's host is unknown".
    sourceAttachedTo :: Maybe ObjectId.ObjectId,
    -- The object the surrounding Quantity is evaluated against, which
    -- IsAttachedToEvaluated compares a candidate's host with. Filled by
    -- Pawl.Engine.Quantity's Count arm alone; Nothing everywhere else, where the
    -- atom answers False. Pawl.FilterPositionLintSpec's "CR 613.4c no card asks
    -- IsAttachedToEvaluated outside a Count" keeps a card to the filled position.
    evaluated :: Maybe ObjectId.ObjectId,
    -- CR 400.7: the objects an effect of the SOURCE put onto the battlefield, as
    -- those objects, for EnteredWithSource (Animate Dead's enchant ability). Filled
    -- from GameState.enteredWith by Pawl.Engine.SourceContext; empty in contextFor
    -- below, and the atom then matches nothing. LAZY: filling it scans the
    -- relation.
    sourceEntrants :: Set.Set ObjectId.ObjectId,
    -- CR 108.3: the OWNER of the SOURCE, for the one atom that compares a
    -- candidate's owner against it (SameOwnerAsSource, CR 702.140a's "with the
    -- same owner as this spell"). Off Object.owner rather than off `perspective`:
    -- CR 109.5's "you" is the CONTROLLER, a different player for a spell cast off
    -- somebody else's card. Filled by Pawl.Engine.SourceContext; the atom is
    -- MINTED by the rule (Pawl.Engine.Keyword.mutateTarget) into a target slot,
    -- and Pawl.FilterPositionLintSpec's "CR 702.140a no card writes a
    -- source-owner comparison" keeps card data from writing it.
    --
    -- Nothing in contextFor below and for a source the state no longer holds,
    -- and the atom answers False on that Nothing.
    sourceOwner :: Maybe PlayerId.PlayerId,
    -- CR 201.4: the names the SOURCE has chosen, for the one atom that compares a
    -- candidate's against them (HasChosenName, Ancient Vendetta). Filled by
    -- Pawl.Engine.SourceContext with the source's other choices, so every
    -- position framed by its source answers it: a resolution's (CR 608.2c's
    -- choice during a resolution -- Ancient Vendetta's search, Predict's tally,
    -- Petra Sphinx's revealed card) and a replacement row's, where rule
    -- 702.16e's MINTED shield writes it (CR 614.1c's as-enters choice, Runed
    -- Halo), among them. Pawl.Engine.Target.slotContext empties it again: a
    -- target slot is matched at CR 601.2c, before a resolution's choice.
    --
    -- The SOURCE's and not a slot's, which is what separates it from slotNames
    -- above: CR 201.4's name is not an object, so no slot ever holds it, and
    -- Pawl.Types.Object.chosenNames is where both this and CR 614.1c's as-enters
    -- twin put it.
    --
    -- Read LIVE off the board rather than captured when the resolution began: CR
    -- 608.2c has the controller follow the instructions in order, so Ancient
    -- Vendetta's search sees the name its own earlier clause chose. That is the
    -- stale-read shape this field exists on the far side of.
    --
    -- EMPTY in contextFor below, so the atom is vacuously False wherever no
    -- source frames the read -- an empty intersection, which is this atom's arm
    -- answering rather than a posture the record enforces; slotControllers'
    -- atom answers True on ITS unfilled read. What keeps a card out of those
    -- positions is Pawl.FilterPositionLintSpec's "CR 201.4 no card asks
    -- HasChosenName outside an admitted position". That lint is an ALLOWLIST,
    -- narrower than where this field is filled: it refuses every standing
    -- position, which is what Pawl.CardSpec's StandingHostFramed exists to keep
    -- apart from an effect's own ObjectRef.
    sourceChosenNames :: Set.Set CardName.CardName,
    -- CR 702.16k: the player chosen (CR 614.1c) by the permanent whose
    -- PROTECTION ability wrote the filter being matched, for the one atom that
    -- asks after them (OfChosenPlayer, True-Name Nemesis). Supplied by the
    -- caller for slotNames' reason, and by the four positions rule 702.16 reads a
    -- quality in: Pawl.Engine.Replacement.candidateContext (rule 702.16e's
    -- shield), Pawl.Engine.Target.targetable (rule 702.16b),
    -- Pawl.Engine.AttachRestriction.barredBy (rules 702.16c and 702.16d) and
    -- Pawl.Engine.CombatRestriction.cantBeBlockedBy (rule 702.16f).
    --
    -- The CARRIER's and not `source`'s, which is what separates it from
    -- sourceChosenNames above: in three of those four positions the carrier IS
    -- the context's source, but rule 702.16b's quality is matched AGAINST the
    -- aiming object, so Pawl.Engine.Target puts that object in `source` and the
    -- protected candidate here.
    --
    -- Read LIVE off the board, sourceChosenNames' posture: CR 609.7b rechecks a
    -- prevention shield's source half at the event, and a permanent's static
    -- ability is asked afresh every time.
    --
    -- Nothing in contextFor below, so the atom is vacuously False in every position
    -- but those four -- sourceAttachedTo's posture rather than slotControllers'.
    -- What keeps a card out of the other positions is Pawl.CardSpec's "CR 702.16k
    -- no card asks OfChosenPlayer outside a keyword's own filter", the sweep
    -- sourcePower's, slotNames', sourceAttachedTo's and sourceChosenNames'
    -- siblings each have.
    carrierChosenPlayer :: Maybe PlayerId.PlayerId,
    -- CR 702.16b / CR 702.16k: the controller of the spell or ability being
    -- AIMED, which rule 702.16k's targeting clause names beside the source
    -- object's -- "can't be targeted by spells or abilities the specified player
    -- controls". Read by the two atoms that ask who an object belongs to
    -- (OfChosenPlayer, OfRelatedPlayer), and filled by the one position that
    -- judges a protection quality against an aiming object,
    -- Pawl.Engine.Target.targetable, for a permanent and (through
    -- Pawl.Engine.PlayerEffect.protectedFromGiven) a player alike.
    --
    -- Nothing in the other three positions rule 702.16 reads a quality in, and
    -- rightly: its damage clause names "sources controlled by the specified
    -- player", and its Aura, Equipment and blocking clauses name objects that
    -- player controls, so each of those judges the OBJECT the atom is matched
    -- against -- which is what the atom's own arm reads when this is unfilled.
    --
    -- NESTED, because the two Maybes answer different questions: the outer says
    -- whether an aiming spell or ability frames the match at all, and the inner
    -- is CR 109.5's "you" for it, which the caller may not know (see
    -- Pawl.Engine.Target's `perspective`). An unknown "you" names nobody and
    -- adds nothing to the source's two halves, the vacuous posture every
    -- player-referencing question here takes.
    --
    -- Not derivable from the aiming object's own view: CR 113.8 with CR 109.5
    -- fix an activated ability's controller as the player who activated it, so
    -- a source stolen in response keeps its activator here. The
    -- cast-prohibition scenario "an opponent's ability still can't target the
    -- protected player once she steals its source in response" proves it.
    aimingController :: Maybe (Maybe PlayerId.PlayerId),
    -- CR 105.2: the colours the SOURCE chose as it entered (CR 614.1c), for the
    -- atom that asks whether a candidate wears one (HasChosenColor, Gauntlet of
    -- Power) and the quantity that counts them (Quantity.ChosenColorsItIs). CR 607.2d links the choosing ability to every ability printed
    -- beside it that names "the chosen color", so every position framed by its
    -- source fills it: Pawl.Engine.SourceContext fills every choice at once
    -- (Caged Sun's affected set, Pentarch Paladin's target slot, Kindred
    -- Discovery's trigger condition, Tablet of the Guilds' intervening "if",
    -- Brass Herald's resolution, Doom Cannon's cost). CR 106.6's mana restriction reads it off the
    -- MANA UNIT instead (Pawl.Engine.Mana.admitsUnder, Throne of Eldraine), for
    -- sourceChosenSubtype's reason below. A static grant's bare protection quality
    -- (Cho-Manno's Blessing) never reaches it: Pawl.Engine.Keyword.grantedBy
    -- bakes the granter's choice out.
    --
    -- The SOURCE's, sourceChosenNames' direction rather than carrierChosenPlayer's:
    -- the permanent whose ability asks is the permanent that made the choice, and
    -- the candidate is what the filter is matched against. Object.chosenColors is
    -- per-incarnation (CR 707.6 does not copy it), so two Gauntlets naming two
    -- colours answer differently on the one board.
    --
    -- Empty in contextFor below, so the atom is vacuously False wherever no
    -- source frames the read. What keeps a card out of those positions is
    -- Pawl.FilterPositionLintSpec's "CR 607.2d no card asks HasChosenColor or
    -- HasChosenSubtype outside an admitted position".
    sourceChosenColors :: Set.Set Color.Color,
    -- CR 205.3: the subtype the SOURCE chose as it entered (CR 614.1c), for the
    -- one atom that asks whether a candidate wears it (HasChosenSubtype). Filled
    -- where sourceChosenColors above is, and read the same way (Obelisk of Urd,
    -- From the Rubble, Doom Cannon).
    --
    -- One more caller, which reads it from elsewhere: Pawl.Engine.Mana.admitsUnder
    -- reads it off the MANA UNIT, because a CR 106.6 restriction is asked when the
    -- source may be gone: CR 106.6a makes the restriction the ability's, so the
    -- answer is the one baked in when the mana was produced
    -- (Pawl.Types.ManaUnit.sourceChoices, Pillar of Origins). That caller
    -- overrides whatever this field holds.
    --
    -- Vacuously False everywhere else, sourceChosenColors's posture, and fenced by
    -- the same lint's subtype twin.
    sourceChosenSubtype :: Maybe Subtype.Subtype,
    -- CR 607.2a: the last card exiled with the source, for IsLastExiledWithSource.
    -- Filled by one caller only, Pawl.Engine.Mana.admitsUnder, off the MANA UNIT
    -- (Pawl.Types.ManaUnit.sourceLastExiled, Ice Cauldron) for sourceChosenSubtype's
    -- reason. Nothing everywhere else, so the atom is vacuously False there, and
    -- Pawl.FilterPositionLintSpec keeps a card to a mana restriction.
    sourceLastExiled :: Maybe ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)

-- A Context for every match whose Filter cannot name a context-relative atom --
-- that is, every match but a target slot's, CR 702.149a's trigger condition and
-- CR 509.1b's blocking gate. The OfSource operand reaches a card only through
-- Pawl.Engine.Keyword's own minted abilities, Pawl.Engine.Ring's emblem, a wish's
-- filter (Pawl.Engine.Event.eligible fills the source there), a pairwise combat
-- restriction and a triggered ability's own condition, and CR 702.39a's
-- defending-player atom only through provoke; Pawl.FilterPositionLintSpec's
-- lints keep both out of every other position in card data, so no other
-- position can read the Nothings this leaves.
--
-- CR 303.4b's host atom is the second one a CARD may write (Ray of Frost,
-- Oppressive Rays), and it reads a Nothing here in every position but the ones
-- that supply it -- see sourceAttachedTo above for the list and for the lint that
-- keeps a card to it.
--
-- CR 119.5's recipient atom is the one a CARD may write (Biorhythm), so the
-- Nothing it leaves here is reachable: a card naming "they control" outside a
-- per-recipient effect matches nothing, which is PlayerRef.Candidate's posture one
-- type over rather than a hole -- an evaluation that has reached no recipient has
-- no honest player to substitute.
--
-- CR 201.4's chosen-name atom is a further one a CARD may write (Ancient
-- Vendetta, Predict, Petra Sphinx), and it reads the empty set here in every
-- position but the ones that supply it -- Pawl.Engine.SourceContext's, every
-- position of a resolution among them, and
-- Pawl.Engine.Replacement.candidateContext, which fills it for a minted row. See
-- that field above for the lint that keeps a card to a subset of the first.
--
-- CR 702.16k's chosen-player atom (True-Name Nemesis) is a further one a CARD may
-- write, and it reads the Nothing here in every position but the four rule 702.16
-- reads a protection quality in -- see carrierChosenPlayer above for the list and
-- for the lint that keeps a card to them.
--
-- CR 105.2's chosen-colour atom (Gauntlet of Power) and CR 205.3's chosen-subtype
-- atom (Obelisk of Urd) are two more a CARD may write, and they read the Nothing
-- here in every position but the ones CR 607.2d links the choice to -- see
-- sourceChosenColors above for the list and for the lint that keeps a card to them.
--
-- CR 607.2a's last-exiled atom (Ice Cauldron) is one more, read as Nothing here
-- in every position but a CR 106.6 restriction -- see sourceLastExiled above.
--
-- CR 110.2's same-controller atom (Bioshift) is the one whose unfilled read does
-- NOT match nothing, so the paragraphs above are a finding per atom rather than
-- one argument about this record: `slotControllers` is empty here, and the
-- arm in `matches` reads a slot with no key as a relation with only one party and
-- ADMITS every candidate. Nothing narrows an offer written against that empty
-- map; what keeps a card out of the positions it is empty in is Pawl.CardSpec's
-- "CR 110.2 no card asks SameControllerAsBound outside a mode's target slot or an attach destination"
-- alone, Pawl.Engine.Target.slotContext and Pawl.Engine.Resolve.Slots.effectContext
-- being the only fillers. An atom added
-- here owes both halves of the same pair: which way its unfilled read answers,
-- and what holds a card to the positions that fill it.
contextFor :: Teams.Teams -> Maybe PlayerId.PlayerId -> Maybe ObjectId.ObjectId -> Context
contextFor t p s = MkContext {teams = t, perspective = p, source = s, sourcePower = Nothing, sourceToughness = Nothing, sourceManaValue = Nothing, sourceColors = Set.empty, sourceNames = Set.empty, slotAmount = Nothing, defendingPlayers = [], recipient = Nothing, slotObjects = Map.empty, cantCrewVehicles = Set.empty, slotNames = Map.empty, slotControllers = Map.empty, slotHostControllers = Map.empty, subjectHostCardTypes = Set.empty, slotCreatureTypes = Map.empty, slotMeasures = Map.empty, slotPlayers = Map.empty, boundAmounts = Map.empty, slotStickers = Map.empty, boundUnannounced = False, sourceAttachedTo = Nothing, evaluated = Nothing, sourceEntrants = Set.empty, sourceOwner = Nothing, sourceChosenNames = Set.empty, carrierChosenPlayer = Nothing, aimingController = Nothing, sourceChosenColors = Set.empty, sourceChosenSubtype = Nothing, sourceLastExiled = Nothing}

-- The ONE object a slot names, for the readers that can take no more than one --
-- Quantity.AgainstSlot's evaluation, Count's IsControllerOfBound. Nothing where
-- the slot names nothing AND where it names several: a reader that cannot take a
-- group must not silently take one of its members, which is the doctrine
-- Pawl.Engine.Binding.onlyOne states one type over. Filter.IsBound is the reader
-- that CAN take them, and it goes to `slotObjects` itself.
--
-- Over the WHOLE slot's objects, where Binding.oneBySlot reads its targets alone,
-- so a group of one answers its member here and a slot naming a player beside one
-- object answers the object. No card reaches either: Binding.oneBySlot's note
-- names the two lints that keep a singular read off such a slot.
slotOneObject :: SlotName.SlotName -> Context -> Maybe ObjectId.ObjectId
slotOneObject slot context = case Set.toList (Map.findWithDefault Set.empty slot (slotObjects context)) of
  [oid] -> Just oid
  _ -> Nothing

-- CR 208.1 / 202.3: the number a Measure names on a view, Nothing where the
-- object has none (CR 208.3's noncreature, a player).
measureOf :: Measure.Measure -> View -> Maybe Integer
measureOf measure = case measure of
  Measure.Power -> power
  Measure.Toughness -> toughness
  Measure.ManaValue -> manaValue

-- What a Measures atom's operand reads: a literal, the candidate's own view, or
-- a number the Context carries, Nothing where the Context carries none.
operandValue :: Context -> View -> Operand.Operand -> Maybe Integer
operandValue context view operand = case operand of
  Operand.Literal n -> Just n
  Operand.Own measure -> measureOf measure view
  Operand.OfSource measure -> case measure of
    Measure.Power -> sourcePower context
    Measure.Toughness -> sourceToughness context
    Measure.ManaValue -> sourceManaValue context
  Operand.OfBound bound -> Map.lookup (BoundMeasure.slot bound, BoundMeasure.measure bound) (slotMeasures context)
  Operand.AmountInSlot slot -> toInteger <$> Map.lookup slot (boundAmounts context)
  Operand.EnclosingAmount -> slotAmount context

-- Does the measured number relate thus to the threshold? Pawl.Types.Comparison's
-- one reader: this module's Measures atom and Pawl.Engine.Condition's Compares.
compares :: Comparison.Comparison -> Integer -> Integer -> Bool
compares comparison n t = case comparison of
  Comparison.Exactly -> n == t
  Comparison.AtLeast -> n >= t
  Comparison.AtMost -> n <= t
  Comparison.LessThan -> n < t
  Comparison.GreaterThan -> n > t

-- The one generic matcher. A pure fold over the Filter tree; it never inspects
-- which effect produced the Filter. Identity checks like IsSource consult the
-- supplied Context, not information baked into the predicate.
matches :: Context -> View -> Filter.Filter Keyword.Type.Keyword -> Bool
matches context view predicate = case predicate of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.IsMonocolored -> Set.size (colors view) == 1
  -- CR 702.78a's "share a color with it", the arm above asked of two objects:
  -- CR 105.2 makes colour a SET, so sharing is a non-empty intersection. False
  -- where either side is colourless, which is that reading rather than an
  -- absent-value convention -- a colourless object shares no colour with
  -- anything, and a context that supplied no colours names none to share.
  Filter.SharesColorWithSource -> not (Set.disjoint (colors view) (sourceColors context))
  Filter.HasSubtype s -> Set.member s (subtypes view)
  -- CR 709.4a's own test, said the way that rule says it: membership, so an
  -- object showing several names matches on any one of them.
  Filter.HasName n -> Set.member n (names view)
  Filter.NameWordsAtLeast n -> any (\named -> NameWords.wordCount named >= n) (names view)
  -- The same membership test, against the set CR 206.3 defines rather than one
  -- name the card gives. ANY of the candidate's names, which is CR 709.4a's
  -- reading exactly as HasName's is -- Pawl.FilterSpec's "CR 709.4a matches an
  -- object showing a listed name among others" is what proves it is not every
  -- one. False for a code the rule does not name, which
  -- Pawl.Codec.Expansion.make keeps out of card data.
  Filter.HasNameOriginallyPrintedIn e -> case Expansion.names e of
    Nothing -> False
    Just listed -> any (\name -> Set.member name listed) (names view)
  -- CR 702.1 / CR 109.3: abilities ARE a characteristic, so this is the same kind
  -- of read HasCardType is -- off the projection where there is one, which is what
  -- makes "target creature with flying" (Plummet, CR 702.9) track a grant and a
  -- Humility alike rather than the printed type line.
  Filter.HasKeyword k -> Set.member k (keywords view)
  -- CR 702.164a and CR 702.14a: the same read one step coarser, asking which
  -- keyword each ability IS rather than how it is written -- so Flensing Raptor's
  -- "creature you control with toxic" reaches toxic 1 and toxic 3 alike.
  --
  -- SCANNED rather than looked up, because the projection is keyed by the whole
  -- keyword and a family is not a key. Nothing stores the families beside them:
  -- a derived set that some later code sampled and others recomputed is this
  -- repository's recurring bug, and the set being scanned is a single object's
  -- abilities, never the board.
  Filter.HasKeywordFamily f ->
    any ((== Just f) . Keyword.familyOf) (keywords view)
  -- CR 208.1 / 202.3: one comparison, whatever is measured and whatever it is
  -- measured against. False unless BOTH numbers are readable: an object with no
  -- power (CR 208.3) is not "a creature with power 2 or less", and an operand
  -- nothing supplied -- a source no context projected, a slot naming no amount
  -- or no single object -- names no bound to compare with.
  --
  -- The one exception is an EnclosingAmount CR 601.2b has not announced yet,
  -- which states nothing and so narrows nothing (boundUnannounced): the
  -- permissive direction keeps the castability gate from refusing a spell the
  -- announcement could still make legal. A candidate with no measure is still
  -- excluded.
  Filter.Measures m -> case (measureOf (Measures.measure m) view, operandValue context view (Measures.operand m)) of
    (Just n, Just t) -> compares (Measures.comparison m) n t
    (Just _, Nothing) | Measures.operand m == Operand.EnclosingAmount -> boundUnannounced context
    _ -> False
  -- CR 202.3 again, read for parity. Void Winnower's reminder text is the
  -- boundary case in the rulebook's own words -- "(Zero is even.)" -- and `even
  -- 0` agrees, which is also CR 202.3a's answer for an object with no mana cost:
  -- an animated land has a mana value of 0 and so an EVEN one.
  --
  -- Vacuously False where there is no mana value at all, exactly as the atom
  -- above is: a player, or an event snapshot carrying none. An ability on the
  -- stack is NOT that case -- CR 202.3a's 0 is even, and it matches.
  Filter.ManaValueIsEven -> maybe False even (manaValue view)
  -- PlayerRelation.holds is what each arm MEANS, and its haddock carries the
  -- argument: an Opponent is CR 102.3's player not on your team, which is every
  -- other player in a free-for-all (CR 806.1) and at two seats (CR 102.2), and
  -- AnyPlayer admits the perspective too. Unlike
  -- Pawl.Engine.Count.playersFor, which folds a player SET, this arm tests one
  -- candidate `View` at a time, so there is no set here to get the size of wrong.
  --
  -- A perspective is demanded even for AnyPlayer, which does not read one. That
  -- is the whole atom's posture rather than this arm's: a match with no
  -- perspective is one nothing has framed, and answering it True off a relation
  -- that happens not to need one would make the atom mean something different
  -- depending on which relation it carries.
  Filter.ControlledBy relation -> case (controller view, perspective context) of
    (Just c, Just p) -> PlayerRelation.holds (teams context) relation p c
    _ -> False
  -- CR 508.5 / 702.39a: the candidate's controller IS the defending player, which
  -- the Context supplies because it is a fact about the combat record rather than
  -- about the candidate -- or, for a source with no attack, IS ONE of the
  -- defending players (CR 802.2a, Yare). False with no controller or no defending
  -- player, the posture the Measures atom takes.
  Filter.ControlledByDefendingPlayer -> case controller view of
    Just c -> List.elem c (defendingPlayers context)
    Nothing -> False
  -- CR 603.2's "that player controls". `bakeBound` below replaces the atom before
  -- either of CR 115's moments judges the slot; a COUNT is matched unbaked, so the
  -- atom reads the Context's slotPlayers there -- Mana Cache's "for each untapped
  -- land that player controls" (Pawl.ManaSpec's Mana Cache group). False unless the
  -- slot names exactly one player, bakeBound's own posture and the vacuous one
  -- every player-referencing atom takes.
  Filter.ControlledByBound slot -> case fmap Set.toList (Map.lookup slot (slotPlayers context)) of
    Just [pid] -> controller view == Just pid
    _ -> False
  -- The baked half: the candidate's controller IS this player, with no perspective
  -- to relate it to. Vacuously False off an object, `controller` being Nothing for a
  -- player view and for a card in a hidden zone (CR 108.4).
  Filter.ControlledByPlayer pid -> controller view == Just pid
  -- The candidate's controller IS the recipient the effect has currently reached,
  -- which the Context supplies because it is a fact about how far the surrounding
  -- effect has got rather than about the candidate. False unless both are
  -- readable, ControlledByDefendingPlayer's posture: a candidate with no
  -- controller has nothing to compare, and no recipient at all is every position
  -- but a per-recipient effect's quantity.
  Filter.ControlledByRecipient -> case (controller view, recipient context) of
    (Just c, Just r) -> c == r
    _ -> False
  -- CR 108.3 / 110.2: the same comparison ControlledBy makes, against the other
  -- player -- so Garland's "creatures you control but don't own" is the two atoms
  -- conjoined. An Opponent is CR 102.3's, for the reason the arm above gives.
  -- Vacuously False where no object backs the view, or where no
  -- perspective frames the match.
  Filter.OwnedBy relation -> case (owner view, perspective context) of
    (Just o, Just p) -> PlayerRelation.holds (teams context) relation p o
    _ -> False
  -- ControlledByRecipient's comparison against CR 108.3's owner, for its
  -- reasons: False unless both are readable.
  Filter.OwnedByRecipient -> case (owner view, recipient context) of
    (Just o, Just r) -> o == r
    _ -> False
  Filter.IsSource -> case (identity view, source context) of
    (Just oid, Just src) -> oid == src
    _ -> False
  -- The baked half: the candidate IS this object, with no source to relate it to.
  -- Vacuously False off an object, `identity` being Nothing for a player view.
  Filter.IsObject named -> identity view == Just named
  -- CR 115.1 the other way round from IsSource: the SOURCE is among what the
  -- candidate targets. Every object-shaped Recipient counts (Recipient.objectOf),
  -- CR 115.4's "any target" naming a creature, planeswalker or battle by tag.
  -- Vacuously False where no source frames the match or the candidate targets
  -- nothing.
  Filter.TargetsSource -> case source context of
    Just src -> any ((== Just src) . Recipient.objectOf) (targets view)
    Nothing -> False
  -- The atom above with "only" on it, so ARITY is asked as well as membership:
  -- every one of the candidate's targets is the source. A player target answers
  -- Nothing to Recipient.objectOf and so fails, which is the rule -- "targets
  -- only Zada" is false of a spell that also names a player. Vacuously False
  -- where the candidate targets nothing at all, which the emptiness guard is:
  -- `all` over an empty set is True and a permanent targets nothing.
  Filter.TargetsOnlySource -> case source context of
    Just src -> not (Set.null (targets view)) && all ((== Just src) . Recipient.objectOf) (targets view)
    Nothing -> False
  -- The atom above asked by DESCRIPTION, and ARITY is the whole of what "a
  -- single" adds: exactly one recipient, and the nest matched against THAT
  -- recipient's own view rather than against the tag CR 601.2c wrote on the
  -- spell's slot. Needs no source, which is why it can sit where
  -- TargetsOnlySource cannot.
  --
  -- The nest is judged in the SAME context the candidate is, so "a single
  -- creature YOU control" (Leyline of Resonance) reads the perspective the
  -- trigger's controller frames.
  --
  -- False where the one recipient has no view: CR 608.2b's gone target, which
  -- answers no description at all.
  Filter.TargetsOnlyOne f -> case Set.toList (targets view) of
    [r] -> maybe False (\target -> matches context target f) (Map.lookup r (targetViews view))
    _ -> False
  -- Arity alone, and counted per instance rather than per recipient: a spell
  -- aimed at one creature through two "target" words has two targets, which
  -- Deflection's ruling refuses ("targets the same player or object multiple
  -- times") and TargetsOnlyOne above admits. Pawl.TargetSpec's Deflection group
  -- proves it.
  Filter.HasSingleTarget -> targetCount view == 1
  -- The atom above without "only": ANY target's view the nest matches, in the
  -- same context, so "a permanent YOU control" (Rebuff the Wicked) reads the
  -- evaluating source's controller and never the candidate's. CR 115.9b ignores
  -- a target that left its zone, which targetViews' dropped key is.
  Filter.TargetsMatching f -> any (\target -> matches context target f) (targetViews view)
  -- CR 115.1's player target, judged against the perspective the way ControlledBy
  -- judges a controller. ONLY a ToPlayer counts: CR 115.10a says an object is a
  -- target only where the word names it, and a spell aimed at a creature names
  -- the creature and not its controller.
  Filter.TargetsPlayer relation -> case perspective context of
    Just p -> any (\r -> case r of Recipient.ToPlayer q -> PlayerRelation.holds (teams context) relation p q; _ -> False) (targets view)
    Nothing -> False
  -- IsSource one field over: the id the RESOLUTION bound rather than the id the
  -- evaluation is sourced at. Vacuously False for a view with no object behind
  -- it and for a slot naming nothing, which is the posture the atom above takes.
  --
  -- MEMBERSHIP, so a slot bound to a GROUP admits every one of its members: CR
  -- 701.17c's "from among them" is a question about the whole batch a mill,
  -- a look or a move named, and a slot naming one object is the singleton case
  -- of it rather than a different question.
  Filter.IsBound slot -> case identity view of
    Just oid -> Set.member oid (Map.findWithDefault Set.empty slot (slotObjects context))
    Nothing -> False
  -- IsBound over the announcement's every target at once (Binding.announcedTargets).
  Filter.IsTarget -> matches context view (Filter.IsBound Binding.announcedTargets)
  -- CR 709.4a at both ends: the candidate has the bound object's name if one of
  -- its names is one of that object's, which is a non-empty INTERSECTION. A slot
  -- naming nothing, and a bound object with no name (CR 708.2a), each leave the
  -- other side empty and answer False without a case of their own.
  Filter.SameNameAsBound slot -> not (Set.disjoint (names view) (Map.findWithDefault Set.empty slot (slotNames context)))
  -- CR 709.4a at both ends again, one operand over: the candidate has the
  -- source's name if the two name sets INTERSECT. A source nothing supplied, and
  -- a source with no name (CR 708.2a), each leave the other side empty and
  -- answer False without a case of their own.
  Filter.SameNameAsSource -> not (Set.disjoint (names view) (sourceNames context))
  -- CR 108.3 compared across the candidate and the SOURCE rather than against CR
  -- 109.5's perspective, which is what separates it from OwnedBy above: rule
  -- 702.140a's mutate slot asks whose card the SPELL is, and a spell cast off
  -- somebody else's card answers a different player than its controller. A source
  -- nothing supplied, and a candidate with no owner (CR 109.1), each answer False.
  Filter.SameOwnerAsSource -> case (owner view, sourceOwner context) of
    (Just o, Just p) -> o == p
    _ -> False
  -- CR 110.2 compared across two of one announcement's targets, and the one atom
  -- here that is vacuously TRUE rather than False: a slot the context has no key
  -- for is one nothing has named yet, and a relation with only one party to it
  -- constrains nothing. See the atom's own note for why CR 601.2c makes that the
  -- honest answer -- Pawl.Engine.Target.legalSetsGiven's first pass offers the
  -- union and selectionLegal's joint check is what narrows. A key that is present
  -- and EMPTY is the other case: a bound object with no controller (CR 108.4), and
  -- nothing shares a controller with it.
  Filter.SameControllerAsBound slot -> case Map.lookup slot (slotControllers context) of
    Nothing -> True
    Just pids -> maybe False (`Set.member` pids) (controller view)
  -- CR 110.2 asked of the bound object's HOST (CR 303.4b), the atom above's
  -- comparison one link along and with the opposite vacuous direction: an
  -- absent key answers False, since an attach destination has no joint check
  -- behind it to narrow a widened offer. A key that is present and EMPTY is a
  -- bound object attached to nothing, to a player, or to a host that has left
  -- the battlefield, and nothing shares a controller with any of those.
  Filter.SameControllerAsHostOfBound slot -> maybe False (`Set.member` Map.findWithDefault Set.empty slot (slotHostControllers context)) (controller view)
  -- CR 205.3m at both ends, SameNameAsBound's intersection: the context holds
  -- only the bound objects' creature types, so a shared land type (Dryad Arbor's
  -- Forest) is not a match. VACUOUSLY TRUE where the slot is unbound,
  -- SameControllerAsBound's posture and for its reason: CR 601.2c's offer is
  -- made before the sibling target is chosen, and
  -- Pawl.Engine.Target.selectionLegal's joint check narrows it (Unbury's "two
  -- target creature cards that share a creature type").
  Filter.SharesCreatureTypeWithBound slot -> case Map.lookup slot (slotCreatureTypes context) of
    Nothing -> True
    Just types -> not (Set.disjoint (subtypes view) types)
  -- CR 201.4 at both ends, the arm above's INTERSECTION for CR 201.4g's reason as
  -- much as CR 709.4a's: choosing one of a set of interchangeable names chooses
  -- each of them, so a candidate showing either matches. A source that has chosen
  -- nothing, and a candidate with no name at all (CR 708.2a), each leave one side
  -- empty and answer False without a case of their own.
  Filter.HasChosenName -> not (Set.disjoint (names view) (sourceChosenNames context))
  -- CR 105.2 read off the PROJECTION, the HasColor arm above with the colour
  -- arriving on the Context: whatever layer 5 left the candidate wearing is what
  -- CR 607.2d's link is compared against, so Painter's Servant's blue reaches this
  -- as readily as a printed cost does. A source that has chosen none matches
  -- nothing.
  Filter.HasChosenColor -> not (Set.disjoint (sourceChosenColors context) (colors view))
  -- CR 205.3 read off the PROJECTION, the HasSubtype arm above with the subtype
  -- arriving on the Context: whatever CR 613.1d's layer left the candidate
  -- wearing is what CR 607.2d's link is compared against. A source that has
  -- chosen none matches nothing.
  Filter.HasChosenSubtype -> maybe False (`Set.member` subtypes view) (sourceChosenSubtype context)
  -- CR 607.2a off the Context, which the mana-spending road alone fills: no
  -- card exiled with the source matches nothing.
  Filter.IsLastExiledWithSource -> maybe False (\card -> identity view == Just card) (sourceLastExiled context)
  -- CR 702.16k's two halves in one disjunction: what the chosen player CONTROLS,
  -- and what they OWN that no other player controls -- which is the second half's
  -- whole content, CR 108.4 leaving a card outside the battlefield and the stack
  -- with no controller at all. A carrier that chose nobody matches nothing.
  --
  -- No characteristic is read, deliberately: the rule ends "regardless of that
  -- object's characteristic values", so this arm is the one quality that asks who
  -- an object belongs to rather than what it looks like.
  --
  -- Rule 702.16k's TARGETING clause adds a third disjunct: the controller of the
  -- spell or ability doing the aiming, which is not the object this is matched
  -- against at all. It widens the two halves rather than replacing them: rule
  -- 702.16b already bars an ability from a SOURCE the player controls, and the
  -- True-Name Nemesis ruling reads the protection as "from each object
  -- controlled by that player". See aimingController.
  Filter.OfChosenPlayer -> case carrierChosenPlayer context of
    Nothing -> False
    Just pid -> ofPlayer (== Just pid) (aimingController context) view
  -- Rule 702.16k's reading per player, for rule 702.16i's "each of your
  -- opponents" (Absolute Virtue): the relation stands in for the chosen player,
  -- read against the perspective, the protection carrier's controller.
  Filter.OfRelatedPlayer relation -> case perspective context of
    Nothing -> False
    Just you -> ofPlayer (maybe False (PlayerRelation.holds (teams context) relation you)) (aimingController context) view
  -- CR 115.1's "target opponent". The same CR 102.3 reading the ControlledBy arm
  -- above argues for. Vacuously False for an object candidate,
  -- which has no playerIdentity, and for a match with no perspective.
  Filter.IsPlayer relation -> case (playerIdentity view, perspective context) of
    (Just candidate, Just you) -> PlayerRelation.holds (teams context) relation you candidate
    _ -> False
  -- The controller of the object a slot names, and False WHEREVER IT IS REACHED,
  -- for ControlsMoreThanYou's reason below: Pawl.Engine.Count.bakePerspective
  -- answers it against the board, and this module holds none. An atom that
  -- survives to here is one in a position nothing bakes -- any filter but a
  -- Scope.OverPlayers count's.
  Filter.IsControllerOfBound _ -> False
  -- CR 110.2's board comparison, and False WHEREVER IT IS REACHED:
  -- Pawl.Engine.Count.bakePerspective replaces the
  -- atom with a trivially true or trivially false predicate before the candidate
  -- is matched, because answering it means counting permanents and this module
  -- holds no game state. An atom that survives to here is one in a position
  -- nothing bakes -- any filter but a Scope.OverPlayers count's.
  Filter.ControlsMoreThanYou _ _ -> False
  -- CR 400.1's per-player graveyard, and False WHEREVER IT IS REACHED, for the
  -- two atoms above's reason plus one of its own: this module holds no game
  -- state to size a zone with, and CR 109.3 counts no zone among an OBJECT's
  -- characteristics, so a candidate that is not a player has nothing to answer
  -- with either. Pawl.Engine.Count.bakePerspective answers it for a player.
  Filter.CardsInGraveyardAtLeast _ -> False
  -- CR 508.1k: a creature stays attacking until it is removed from combat or the
  -- combat phase ends, so this is a live read of the combat record, never a stamp
  -- on the object.
  Filter.IsAttacking -> attacking view
  -- CR 508.1b: the same live read one field over, and the relation is answered
  -- against the perspective here rather than baked, exactly as ControlledBy's is
  -- -- CR 109.5's "you" is what the match is framed by, not a fact about the
  -- candidate.
  --
  -- Vacuously False where either half is unreadable, ControlledBy's posture: a
  -- candidate attacking nothing, or attacking a planeswalker or a battle, has no
  -- player to relate, and a match nothing framed has no "you" to relate it to.
  Filter.IsAttackingPlayer relation -> case (attackingPlayer view, perspective context) of
    (Just a, Just p) -> PlayerRelation.holds (teams context) relation p a
    _ -> False
  -- CR 508.1b: the atom above one arm of AttackTarget over, and the same posture
  -- in every respect -- the relation is answered against the perspective here, and
  -- either half being unreadable is vacuously False.
  --
  -- The seat compared is the planeswalker's CONTROLLER, which
  -- Pawl.Engine.Projection fills through Projection.controllerOf: reading its
  -- OWNER instead would answer a different player for a planeswalker a Confiscate
  -- has moved, which Pawl.CombatEffectSpec's Soul Snare pair is the board for.
  Filter.IsAttackingPlaneswalker relation -> case (attackingPlaneswalkerController view, perspective context) of
    (Just c, Just p) -> PlayerRelation.holds (teams context) relation p c
    _ -> False
  -- CR 310.9d: the last arm of AttackTarget, and the same posture again. The seat
  -- compared is the battle's PROTECTOR, which Pawl.Engine.Projection fills through
  -- Battle.protectorOf: reading the battle's CONTROLLER instead would answer a
  -- different player for every Siege (CR 310.12a), which Pawl.BattleSpec's
  -- Synthetic Bulwark Snare trio is the board for.
  Filter.IsAttackingBattle relation -> case (attackingBattleProtector view, perspective context) of
    (Just protector, Just p) -> PlayerRelation.holds (teams context) relation p protector
    _ -> False
  -- CR 509.1g: the same live read IsAttacking is, off the other map. Never the
  -- question Pawl.Engine.Combat.isBlocked asks: CR 509.1h keeps an attacker
  -- blocked after every creature blocking it has gone, so this can be False for
  -- everything while that is still True.
  Filter.IsBlocking -> blocking view
  -- CR 509.1h: the status the declaration confers, or that an effect confers
  -- (Effect.BecomesBlocked). A live read of the same record, off the keys rather
  -- than the sets -- so this can be True with nothing at all blocking the
  -- creature, which is the case CR 510.1c gives no combat damage.
  Filter.IsBlocked -> blocked view
  -- CR 608.2i: a look-back read of the turn's event log. Unlike IsAttacking it
  -- cannot stop being true within a turn -- nothing removes a GameEvent -- so a
  -- creature removed from combat (CR 506.4) still attacked, which is what
  -- Relentless Assault's "creatures that attacked this turn" means.
  Filter.AttackedThisTurn -> attackedThisTurn view
  -- CR 508.1a: a look-back read of the COMBAT PHASE's record rather than the
  -- turn's log, which is what keeps it apart from the atom above -- CR 511.3
  -- empties it, so CR 500.8's second combat phase starts over.
  Filter.DeclaredAttackerThisCombat -> declaredAttackerThisCombat view
  -- CR 508.3b: the other half of the same record, and the same look-back read.
  -- CR 508.4 keeps a creature put onto the battlefield attacking out of it, so
  -- this is the past-tense "was attacked" and never "is being attacked".
  Filter.DeclaredAttackedThisCombat -> declaredAttackedThisCombat view
  -- CR 509.1a: the same read off the blocking half of that record.
  Filter.DeclaredBlockerThisCombat -> declaredBlockerThisCombat view
  -- CR 701.17a: the same look-back, over the mills rather than the attacks. Like
  -- the atom above it cannot stop being true within a turn, and unlike it the
  -- candidate can stop EXISTING -- CR 400.7 mints a new object the moment the
  -- milled card moves again, and the new one was not milled.
  Filter.MilledThisTurn -> milledThisTurn view
  -- CR 702.122d with CR 101.2: membership of the set the CALLER gathered, the
  -- reading IsBound takes and for its reason -- a prohibition is another
  -- permanent's static ability, so no field of the candidate's own view could
  -- answer it. Vacuously False for a view with no object behind it and for a
  -- context that gathered nothing.
  Filter.CantCrewVehicles -> case identity view of
    Just oid -> Set.member oid (cantCrewVehicles context)
    Nothing -> False
  -- CR 120.1 / 608.2i: the same look-back again, over the damage events. Like
  -- AttackedThisTurn and MilledThisTurn it cannot stop being true within a turn,
  -- and unlike either it is not the reading CR 120.3e's marked damage would give
  -- -- CR 120.6's regeneration and CR 120.3d's wither both leave a creature that
  -- was dealt damage carrying nothing marked. Named rather than counted, since
  -- CantCrewVehicles sits between them and is neither: a prohibition lifts the
  -- moment its source leaves.
  Filter.DealtDamageThisTurn -> dealtDamageThisTurn view
  -- CR 400.7 / 608.2i: the same look-back over the entries. A permanent that
  -- left and came back is a new object whose entry this is; one that phased in
  -- or turned face up made no entry at all (CR 702.26d, CR 708.8).
  Filter.EnteredThisTurn -> enteredThisTurn view
  -- CR 702.122c: the same look-back asked of a RELATION rather than of one
  -- subject -- the candidate's own field says which Vehicles it crewed, and the
  -- SOURCE on the context says which one the card's "it" names. Vacuously False
  -- for a context with no source, where "crewed it" names nothing at all.
  Filter.CrewedSourceThisTurn -> case source context of
    Just src -> Set.member src (crewedThisTurn view)
    Nothing -> False
  -- CR 702.51c: the sibling above's relation one keyword over -- the candidate's
  -- own field says which spells it convoked and which permanents those became,
  -- and the SOURCE on the context says which one the card's "it" names.
  -- Vacuously False for a context with no source, that one's reason.
  Filter.ConvokedSourceThisTurn -> case source context of
    Just src -> Set.member src (convokedThisTurn view)
    Nothing -> False
  -- CR 702.171c: CrewedSourceThisTurn's relation one keyword over, read off
  -- its own field.
  Filter.SaddledSourceThisTurn -> case source context of
    Just src -> Set.member src (saddledThisTurn view)
    Nothing -> False
  -- CR 302.6: not a look-back over the log at all, unlike AttackedThisTurn,
  -- MilledThisTurn and DealtDamageThisTurn -- the engine keeps the answer as
  -- Object.sickness, written at the untap step and cleared whenever control
  -- moves.
  Filter.ControlledSinceTurnBegan -> controlledSinceTurnBegan view
  -- CR 701.3a: a live read of Object.attachedTo and of the host's own projection,
  -- never a stamp on the candidate -- an Aura whose host stops being a creature
  -- stops matching, and CR 704.5m buries it on the next state-based-action pass.
  --
  -- The nest is matched against the HOST's view and the SAME context, which is
  -- what makes CR 109.5's "you" the ability's controller rather than the host's
  -- (Miracle Worker). `And []` nests to "attached to a permanent" (CR 110.1),
  -- because the field is Just only for a host on the battlefield.
  Filter.AttachedTo f -> maybe False (\host -> matches context host f) (attachedToView view)
  -- CR 303.4b / 301.5a with the arrow turned round: a live read of which
  -- permanents name this candidate in their Object.attachedTo, and of each of
  -- their own projections. EXISTENTIAL, which is rule 303.4b's own shape -- one
  -- Aura attached to it makes a creature enchanted, whatever else is.
  --
  -- Each attacher's view is matched against the SAME context, for the reason
  -- AttachedTo's arm gives: CR 109.5's "you" stays the ability's controller, so
  -- `HasAttached (ControlledBy You)` asks after Auras YOU control.
  Filter.HasAttached f -> any (\attacher -> matches context attacher f) (attachedViews view)
  -- CR 701.3a / 301.5a: IsSource's comparison in the other direction -- the
  -- candidate's HOST against the match's source, rather than the candidate itself.
  -- A live read of Object.attachedTo, so an Equipment unequipped by CR 704.5n
  -- stops matching at once. Vacuously False where the candidate is attached to
  -- nothing or to a player, and where no source frames the match.
  Filter.IsAttachedToSource -> case (attachedTo view, source context) of
    (Just host, Just src) -> host == src
    _ -> False
  -- CR 701.3a / 301.5a: the same live comparison against the object the
  -- surrounding quantity is aimed at rather than the match's source -- under CR
  -- 613.4c the affected object, which need not be the source. Vacuously False
  -- where the candidate is attached to nothing or to a player, and where no
  -- quantity aims the match.
  Filter.IsAttachedToEvaluated -> case (attachedTo view, evaluated context) of
    (Just host, Just aimed) -> host == aimed
    _ -> False
  -- CR 303.4b: the same comparison a THIRD way -- the source's host against the
  -- candidate, rather than the candidate's host against the source. A live read
  -- too, one record over: the caller re-reads Object.attachedTo on every match, so
  -- an Aura that has moved names its new host at once. Vacuously False where the
  -- source is attached to nothing or to a player, where no source frames the match,
  -- and where the position does not supply the field at all.
  Filter.IsHostOfSource -> case (identity view, sourceAttachedTo context) of
    (Just oid, Just host) -> oid == host
    _ -> False
  Filter.EnteredWithSource -> maybe False (`Set.member` sourceEntrants context) (identity view)
  -- CR 702.16p's moment is not in any view; Pawl.Engine.Keyword.grantedBy
  -- bakes the atom out of the one position that asks it.
  Filter.AttachedNoLaterThanSource -> False
  -- CR 701.3a: a live read of the legality of the attach this match is framing,
  -- computed by the caller that knows what is moving. Vacuously False outside one.
  Filter.CanHostSubject -> canHostSubject view
  -- CR 701.3a read from the candidate's side, computed by the caller that knows
  -- which host the instruction fixed. Vacuously False outside a search.
  Filter.CanAttachToSubject -> canAttachToSubject view
  -- CR 205.2a read of the attach SUBJECT's host rather than of the candidate,
  -- filled by the caller that knows what is moving. Vacuously False outside an
  -- attach, where the set is empty.
  Filter.HostOfSubjectHasCardType cardType -> Set.member cardType (subjectHostCardTypes context)
  -- CR 111.6: a token isn't a card. A live read of what the object is
  -- represented by (Object.source), never a stamp on the candidate -- and unlike
  -- the two arms above it cannot change while the game runs, because CR 111.3
  -- makes a token's characteristics equivalent to a card's.
  Filter.IsToken -> token view
  -- CR 903.3's designation, read off the owner's deck rather than off the
  -- candidate: `token` above's posture, and immutable for the same kind of
  -- reason -- the designation is made before the game begins (CR 702.124a).
  Filter.IsCommander -> commander view
  -- CR 113.3b against rule 113.3c, read the way IsToken above is: a live read of
  -- what the object IS (Object.source), which no layer rewrites.
  Filter.IsActivatedAbility -> activatedAbility view
  -- CR 113.1c, the atom above widened to both of CR 113.3's kinds, and read the
  -- same way: a live read of what the object IS (Object.source).
  Filter.IsAbility -> ability view
  -- CR 114.5, read off Object.source for the two atoms above's reason: what an
  -- object is represented by is no characteristic, so no layer rewrites it.
  Filter.IsEmblem -> emblem view
  -- CR 113.7: the nest is matched against the SOURCE's view and the same
  -- context, AttachedTo's arm's posture. False where the candidate is no
  -- ability on the stack, since rule 113.7 has nothing to read there.
  Filter.FromSource f -> maybe False (\src -> matches context src f) (abilitySource view)
  Filter.IsTapped -> tapped view
  -- CR 110.5, the same status one category over, and the battlefield scoping is
  -- inside the field for Transformed's reason: see the atom's own comment in
  -- Pawl.Types.Filter.
  Filter.IsFaceDown -> faceDown view
  -- CR 708.12: the nest is matched against the PRINTED CARD's view and the SAME
  -- context, AttachedTo's arm's posture -- CR 109.5's "you" stays the ability's
  -- controller. False where no card represents the candidate, since rule 708.12
  -- has nothing to read there.
  Filter.RepresentedByCard f -> maybe False (\card -> matches context card f) (representedCard view)
  -- CR 406.3, which is NOT the status one line up: CR 110.5d says the two have
  -- no correlation, and the fields differ accordingly.
  Filter.IsExiledFaceDown -> exiledFaceDown view
  -- CR 701.27g, both exclusions inside the field: see the atom's own comment in
  -- Pawl.Types.Filter, and the field below.
  Filter.Transformed -> transformed view
  -- CR 602.1 with CR 605.1a's exclusion, both applied by the builder that holds
  -- the abilities. A live read: the projection is re-asked on every match, so a
  -- land Humility has stripped stops matching at once, and CR 702.29b's and CR
  -- 702.77b's abilities are in the list the builder measured.
  Filter.HasNonManaActivatedAbility -> nonManaActivatedAbility view
  -- CR 602.1 off the same builders, without CR 605.1a's exclusion.
  Filter.HasActivatedAbility -> hasActivatedAbility view
  -- CR 400.1 off Object.zone, a live read like `token` and `tapped` above: the
  -- object moves and the atom answers about where it is NOW. That is what makes
  -- it answer CR 601.2's "from where it is" at a cast gate, which runs before CR
  -- 601.2a's move to the stack.
  Filter.IsInZone z -> zone view == Just z
  -- CR 601.2a off Object.castFrom, which is a STAMP rather than a live read: the
  -- spell it describes has already left the zone named here, so unlike IsInZone
  -- above this atom goes on answering the same way for as long as the spell is on
  -- the stack -- which is what lets CR 601.2f price it; see #2363.
  Filter.WasCastFrom z -> castFrom view == Just z
  -- CR 601.2h off Object.manaSpent, WasCastFrom's posture one record over: a
  -- STAMP written once the payment settled, so it goes on answering the same way
  -- for as long as the object lasts. Vacuously False for a player and for
  -- everything nothing was ever paid for -- `manaSpentTagColors` is empty there.
  Filter.TagWasSpent tag -> Map.member tag (manaSpentTagColors view)
  Filter.Kicked -> any (\(k, n) -> n > 0 && Keyword.isKicker k) (Map.toList (paidCosts view))
  -- CR 701.54e's designation conjunct, asked of the perspective (CR 109.5's
  -- "you"). A live read of Object.ringBearerFor, never a stamp on the candidate:
  -- CR 701.54a ends the designation when another creature takes it, and the next
  -- projection stops matching with nothing to unwind.
  --
  -- Vacuously False with no perspective, the posture ControlledBy and IsPlayer
  -- take: "your Ring-bearer" is unanswerable when there is no "you".
  Filter.IsRingBearer -> case (ringBearerFor view, perspective context) of
    (Just designated, Just you) -> designated == you
    _ -> False
  -- CR 702.95b's "whether a creature is paired", off Object.paired. A live read of
  -- a STORED record, IsRingBearer's posture and not IsAttachedToSource's: what
  -- CR 702.95e ends the pairing for is swept by Pawl.Engine.Soulbond.endWhenBroken
  -- before any player could receive priority, so no reader sees a lapsed row.
  Filter.IsPaired -> Maybe.isJust (paired view)
  -- CR 702.95b's "the creature another creature is paired with", asked as the
  -- candidate's own partner against the match's source. Both creatures carry the
  -- record, so this answers from either side; vacuously False where the candidate
  -- is unpaired or no source frames the match.
  Filter.IsPairedWithSource -> case (paired view, source context) of
    (Just pairing, Just src) -> Pairing.partner pairing == src
    _ -> False
  -- CR 509.1g: the source is among the candidate's blockers. Vacuously False
  -- where no source frames the match.
  Filter.IsBlockedBySource -> maybe False (`Set.member` blockers view) (source context)
  -- The designation, asked of the CANDIDATE. A live read of Object.designations,
  -- never a stamp on the candidate: every rule here ends its designation when the
  -- permanent leaves the battlefield, and CR 400.7's new incarnation simply arrives
  -- without it -- and where a rule states a second ending (CR 702.171b's clock on
  -- saddled) a live read picks that up too, where a stamp would not. Asks nothing of the perspective, unlike the arm above -- none of
  -- these designations belongs to a player. NOT the menace or the can't-block CR
  -- 701.60c hangs off `Suspected` -- a permanent can have either from somewhere
  -- else.
  Filter.HasDesignation d -> Set.member d (designations view)
  -- CR 122.1, asked of the CANDIDATE: has it one or more counters of the kind?
  -- HasDesignation's live read, of counters instead of a designation: CR 400.7's new
  -- incarnation arrives with none, so nothing is stamped on the candidate.
  Filter.HasCounters kind -> Map.findWithDefault 0 kind (counters view) > 0
  -- CR 122.1 again, of the same field and without a kind to look up. `any (> 0)`
  -- and NOT `not (Map.null ...)`: the map is keyed per kind, so a key standing at
  -- zero is a permanent with no counters on it and a null test would answer True
  -- for one. The atom above compares > 0 for the same reason. No door in the
  -- engine writes such a key today -- Pawl.Engine.Event's settleCounters declines
  -- a zero placement outright, and its removeCounters DELETES the key rather than
  -- leaving it at zero -- so that half is a regression fence rather than a
  -- behaviour a board can show. Pawl.FilterSpec's own case says so at the site.
  Filter.HasCountersOfAnyKind -> any (> 0) (Map.elems (counters view))
  -- CR 123.6-123.9: read off the object, never a projection (CR 123.1).
  Filter.HasSticker kind -> elem kind (stickerKinds view)
  -- CR 123.4.
  Filter.Stickered -> not (null (stickerKinds view))
  Filter.And fs -> all (matches context view) fs
  Filter.Or fs -> any (matches context view) fs
  Filter.Not f -> not (matches context view f)

-- CR 612.1: swap subtype words wherever they appear in a Filter. A text-changing
-- effect reaches any word printed on the object, and a Filter carried by an
-- effect is part of that text -- so this is the shape
-- Pawl.Engine.Projection.Rewrite.rewriteModification already has, for the type THIS
-- module owns. Pawl.Engine.Resolve threads one call per Filter-carrying effect
-- arm rather than learning what is inside each one.
--
-- HasSubtype and HasKeyword are the atoms REWRITTEN here. The rest name a card
-- type, a supertype, a colour, a number, a relation, a status, a designation, or a
-- keyword FAMILY, none of which CR 612's word swap reaches -- see the
-- HasKeywordFamily arm below for why the family is in that list while HasKeyword
-- is not. Written
-- out exhaustively rather than with a catch-all, so a later atom that can carry a
-- subtype fails to compile here instead of silently going unrewritten.
--
-- CR 612.2's family gate is not restated on the HasSubtype arm, for the reason
-- Pawl.Engine.Projection's type-line half gives: a HasSubtype atom may name a
-- word of any family, so the family the word is used as IS the family the word
-- belongs to, and the exact `lookup` already asks CR 612.2's question.
rewrite :: [(Subtype.Subtype, Subtype.Subtype)] -> Filter.Filter Keyword.Type.Keyword -> Filter.Filter Keyword.Type.Keyword
rewrite pairs predicate = case predicate of
  Filter.HasSubtype s -> Filter.HasSubtype (Maybe.fromMaybe s (lookup s pairs))
  Filter.And fs -> Filter.And (fmap (rewrite pairs) fs)
  Filter.Or fs -> Filter.Or (fmap (rewrite pairs) fs)
  Filter.Not f -> Filter.Not (rewrite pairs f)
  Filter.HasCardType _ -> predicate
  Filter.HasSupertype _ -> predicate
  Filter.HasColor _ -> predicate
  Filter.IsMonocolored -> predicate
  Filter.SharesColorWithSource -> predicate
  -- Untouched, and CR 612.2 says so outright: "an effect that changes a color
  -- word or a subtype can't change a card name, even if that name contains a
  -- word ... that is the same as a Magic color word, basic land type, or
  -- creature type". This function's pairs are exactly such a subtype swap.
  Filter.HasName _ -> predicate
  Filter.NameWordsAtLeast _ -> predicate
  -- Untouched for HasName's reason, one indirection along: the atom names an
  -- expansion, and the names it stands for are card names too.
  Filter.HasNameOriginallyPrintedIn _ -> predicate
  -- CR 702.14a: a keyword can hold a land-type word too, so "creature with
  -- swampwalk" is text a swap reaches exactly as "creature that's a Swamp" is.
  -- rewriteKeyword below is the descent, shared with the two sites that rewrite
  -- a keyword rather than a filter over one.
  Filter.HasKeyword k -> Filter.HasKeyword (rewriteKeyword pairs k)
  -- DESCENT, like And/Or/Not above: the nested filter describes the permanents
  -- being counted ("more LANDS than you"), so a word swap reaches it exactly as it
  -- reaches the same description written at the top level.
  Filter.ControlsMoreThanYou n f -> Filter.ControlsMoreThanYou n (rewrite pairs f)
  -- Untouched, where the atom above is rewritten, and the contrast is CR 612's
  -- rather than an omission: rule 612.1's swap acts on a WORD in the text, and a
  -- family names no word. Magical Hack turning "Swamp" into "Island" turns a
  -- swampwalk into an islandwalk, so "creature with swampwalk" has to follow it;
  -- "creature with landwalk" still reads landwalk afterwards, and CR 702.14a's
  -- generic term is not itself a land type to swap.
  Filter.HasKeywordFamily _ -> predicate
  -- Untouched: the atom names a comparison, and CR 612.1 finds no word in it to
  -- swap -- nor in a slot name it may carry.
  Filter.Measures _ -> predicate
  Filter.ManaValueIsEven -> predicate
  Filter.ControlledBy _ -> predicate
  -- Untouched for ControlledBy's reason.
  Filter.ControlledByDefendingPlayer -> predicate
  -- Untouched for the same reason twice over: neither a slot name nor a player id
  -- is a word CR 612.1's swap can find in the text.
  Filter.ControlledByBound _ -> predicate
  Filter.ControlledByPlayer _ -> predicate
  -- Untouched for ControlledBy's reason.
  Filter.ControlledByRecipient -> predicate
  -- Untouched for ControlledBy's reason: CR 612.1 swaps a WORD in the text, and
  -- this atom names a player relation rather than a subtype.
  Filter.OwnedBy _ -> predicate
  Filter.OwnedByRecipient -> predicate
  Filter.IsSource -> predicate
  Filter.IsObject _ -> predicate
  -- Untouched for IsSource's reason: a target relation is not a word CR 612.1
  -- swaps.
  Filter.TargetsSource -> predicate
  Filter.TargetsOnlySource -> predicate
  Filter.HasSingleTarget -> predicate
  -- DESCENDED into, unlike the two atoms above: the nest describes an OBJECT and
  -- so may name a subtype -- Precursor Golem's "targets only a single Golem" is
  -- the shape a CR 612.1 swap would find there.
  Filter.TargetsOnlyOne f -> Filter.TargetsOnlyOne (rewrite pairs f)
  Filter.TargetsMatching f -> Filter.TargetsMatching (rewrite pairs f)
  Filter.TargetsPlayer _ -> predicate
  Filter.IsBound _ -> predicate
  Filter.IsTarget -> predicate
  Filter.SameNameAsBound _ -> predicate
  Filter.SameNameAsSource -> predicate
  Filter.SameOwnerAsSource -> predicate
  Filter.SameControllerAsBound _ -> predicate
  Filter.SameControllerAsHostOfBound _ -> predicate
  Filter.SharesCreatureTypeWithBound _ -> predicate
  Filter.HasChosenName -> predicate
  Filter.HasChosenColor -> predicate
  Filter.HasChosenSubtype -> predicate
  Filter.IsLastExiledWithSource -> predicate
  Filter.OfChosenPlayer -> predicate
  Filter.OfRelatedPlayer _ -> predicate
  Filter.IsPlayer _ -> predicate
  -- Untouched for IsPlayer's reason: CR 612.1 swaps a WORD in the text, and this
  -- atom names a slot rather than a subtype.
  Filter.IsControllerOfBound _ -> predicate
  -- Untouched for IsPlayer's reason: CR 612.1 swaps a WORD in the text, and a
  -- card count is a number rather than one. "Graveyard" is a zone name, which
  -- rule 612.1 does not reach either.
  Filter.CardsInGraveyardAtLeast _ -> predicate
  Filter.IsAttacking -> predicate
  Filter.IsAttackingPlayer _ -> predicate
  Filter.IsAttackingPlaneswalker _ -> predicate
  Filter.IsAttackingBattle _ -> predicate
  Filter.IsBlocking -> predicate
  Filter.IsBlocked -> predicate
  Filter.AttackedThisTurn -> predicate
  Filter.DeclaredAttackerThisCombat -> predicate
  Filter.DeclaredAttackedThisCombat -> predicate
  Filter.DeclaredBlockerThisCombat -> predicate
  Filter.MilledThisTurn -> predicate
  Filter.CantCrewVehicles -> predicate
  Filter.DealtDamageThisTurn -> predicate
  Filter.EnteredThisTurn -> predicate
  Filter.CrewedSourceThisTurn -> predicate
  Filter.ConvokedSourceThisTurn -> predicate
  Filter.SaddledSourceThisTurn -> predicate
  -- Untouched for AttackedThisTurn's reason: the atom names no subtype.
  Filter.ControlledSinceTurnBegan -> predicate
  -- DESCENT, for ControlsMoreThanYou's reason above: the nested filter describes
  -- the HOST ("attached to a Swamp"), so CR 612.1's word swap reaches it exactly
  -- as it reaches the same description written at the top level.
  Filter.AttachedTo f -> Filter.AttachedTo (rewrite pairs f)
  -- DESCENT, for the atom above's reason: the nest is a description a card
  -- author wrote ("if it's a Forest card"), so CR 612.1's word swap reaches it
  -- exactly as it reaches the same description written at the top level. The
  -- swap is on the ABILITY's text and never on the card the nest describes.
  Filter.RepresentedByCard f -> Filter.RepresentedByCard (rewrite pairs f)
  -- DESCENT, for the atom above's reason one direction over: the nest describes
  -- the ATTACHER ("enchanted by an Aura"), so CR 612.1's word swap reaches it as
  -- it reaches the same description written at the top level.
  Filter.HasAttached f -> Filter.HasAttached (rewrite pairs f)
  Filter.IsAttachedToSource -> predicate
  Filter.IsAttachedToEvaluated -> predicate
  Filter.IsHostOfSource -> predicate
  Filter.EnteredWithSource -> predicate
  Filter.AttachedNoLaterThanSource -> predicate
  Filter.CanHostSubject -> predicate
  Filter.CanAttachToSubject -> predicate
  Filter.HostOfSubjectHasCardType _ -> predicate
  Filter.IsCommander -> predicate
  Filter.IsToken -> predicate
  Filter.IsActivatedAbility -> predicate
  Filter.IsAbility -> predicate
  Filter.IsEmblem -> predicate
  -- Descended into, HasAttached's reason: the nest describes the SOURCE.
  Filter.FromSource f -> Filter.FromSource (rewrite pairs f)
  Filter.IsTapped -> predicate
  Filter.IsFaceDown -> predicate
  Filter.IsExiledFaceDown -> predicate
  Filter.Transformed -> predicate
  Filter.IsRingBearer -> predicate
  Filter.IsPaired -> predicate
  Filter.IsPairedWithSource -> predicate
  Filter.IsBlockedBySource -> predicate
  Filter.HasDesignation _ -> predicate
  -- Untouched: CR 612.1 swaps a subtype, a colour or a card type word, and this
  -- atom names none -- "an activated ability that isn't a mana ability" has no
  -- word inside it for Artificial Evolution to reach.
  Filter.HasNonManaActivatedAbility -> predicate
  Filter.HasActivatedAbility -> predicate
  Filter.IsInZone _ -> predicate
  -- Untouched for IsInZone's reason: CR 612.1 swaps a subtype, a colour or a card
  -- type word, and a zone name is none of the three.
  Filter.WasCastFrom _ -> predicate
  -- Untouched for IsInZone's reason once more: CR 612.1 swaps a subtype, a colour
  -- or a card type word, and a Pawl.Types.ProductionTag names none of the three --
  -- "mana from an artifact" is a fact about a PAST production event, not a word
  -- on this object for Artificial Evolution to reach.
  Filter.TagWasSpent _ -> predicate
  Filter.Kicked -> predicate
  -- Rewritten THROUGH the kind, for the reason rewriteCounterKind gives.
  Filter.HasCounters kind -> Filter.HasCounters (rewriteCounterKind pairs kind)
  -- Left standing where the atom above is rewritten: CR 612.1 swaps WORDS, and a
  -- kind-agnostic atom names none -- there is no CounterKind here to carry a
  -- subtype word through the swap.
  Filter.HasCountersOfAnyKind -> predicate
  Filter.HasSticker _ -> predicate
  Filter.Stickered -> predicate

-- CR 612.1's word swap inside a COUNTER KIND. Rewritten THROUGH the kind: CR
-- 122.1b's keyword counter carries a keyword, and rule 612.1 reaches a word
-- inside one exactly as it does in Filter.HasKeyword. Every other kind names no
-- word to swap. Exhaustive rather than a wildcard, so a later kind carrying a
-- word fails to compile here instead of silently keeping the printed one.
--
-- Top-level because Pawl.Engine.Projection needs the same rewrite over the
-- counter kinds a replacement effect names (CR 612.1 over CR 604.2's text).
rewriteCounterKind :: [(Subtype.Subtype, Subtype.Subtype)] -> CounterKind.CounterKind Keyword.Type.Keyword -> CounterKind.CounterKind Keyword.Type.Keyword
rewriteCounterKind pairs kind = case kind of
  CounterKind.Keyword k -> CounterKind.Keyword (rewriteKeyword pairs k)
  CounterKind.PlusOnePlusOne -> kind
  CounterKind.MinusOneMinusOne -> kind
  CounterKind.Loyalty -> kind
  CounterKind.Lore -> kind
  CounterKind.Defense -> kind
  CounterKind.Time -> kind
  CounterKind.Age -> kind
  CounterKind.Fade -> kind
  CounterKind.Shield -> kind
  CounterKind.Finality -> kind
  CounterKind.Stun -> kind
  CounterKind.Level -> kind
  CounterKind.Hone -> kind
  -- CR 612.1 swaps a WORD, and this pair is [(Subtype, Subtype)]. A counter's
  -- name is Text and never a Subtype, so no word swap can reach inside one --
  -- not an oversight, and the reason this stays exhaustive rather than becoming
  -- a wildcard.
  CounterKind.Named _ -> kind

-- CR 612.1's word swap INSIDE a keyword. Rule 702 spells some keywords with a
-- word in them: CR 702.14a has landwalk "appear within an object's rules text as
-- '[type]walk'", so the land type in swampwalk is a word in the text box like any
-- other and a text-changing effect reaches it. Magical Hack's own reminder text
-- is that example -- "you may change 'swampwalk' to 'plainswalk'".
--
-- Casing on Keyword is legitimate for the reason Pawl.Types.Keyword's comment
-- gives: rule 702 is part of the rulebook, so a keyword is a citation rather than
-- an effect's identity.
--
-- The whole descent is `rewrite` over the Filters a keyword carries, which
-- answers CR 702.14a's SECOND clause for free -- the [type] "can also be the card
-- type land plus any combination of land types, card types, and/or supertypes",
-- and of those four shapes (CR 702.14c) only a land type is a HasSubtype atom.
-- Vectis Gloves' artifact landwalk and Dryad Sophisticate's nonbasic landwalk come
-- back unchanged because their criteria hold no subtype word, not because this
-- function recognizes which shape it was handed; Legions of Lim-Dûl's snow
-- swampwalk has its Swamp swapped and keeps its Snow, which is the case a
-- shape-aware version would have had to get right on purpose.
--
-- Exhaustive rather than a wildcard, unlike Combat.landwalkAllowsGiven's single
-- named constructor: this CLASSIFIES every keyword by whether it holds a word,
-- so a new one carrying a Filter must break this build rather than silently keep
-- the printed word.
--
-- Every Cost a keyword carries goes through rewriteCost below, for CR 612.1's own
-- reason: rule 702 states those costs as part of the keyword, so they are printed
-- in the text box exactly as an activated ability's activation cost is. No
-- printing pairs one of those costs with a basic land type, so each of those arms
-- is a regression fence rather than a proven path -- Pawl.ActivateSpec's Dark
-- Heart of the Wood is what proves rewriteCost itself.
rewriteKeyword :: [(Subtype.Subtype, Subtype.Subtype)] -> Keyword.Type.Keyword -> Keyword.Type.Keyword
rewriteKeyword pairs keyword = case keyword of
  -- CR 702.14a's "[type]walk".
  Keyword.Type.Landwalk criterion -> Keyword.Type.Landwalk (rewrite pairs criterion)
  -- CR 702.29e's "[Type]cycling", rule 702's other "[type]": "usually a subtype
  -- (as in 'mountaincycling')", so it holds a basic land type exactly as
  -- swampwalk does.
  Keyword.Type.Cycling (Cycling.MkCycling cost criterion) -> Keyword.Type.Cycling (Cycling.MkCycling (rewriteCost pairs cost) (fmap (rewrite pairs) criterion))
  -- CR 702.11d's "hexproof from [quality]", rule 702's third carrier of a word.
  -- Not a "[type]" like the two above -- CR 702.11d's quality is any quality, and
  -- the ones cards actually print tend to name a card type or a colour -- but CR
  -- 612.2 asks the same question of it either way, and `rewrite` answers it: a quality
  -- naming a creature type is "a creature type word used as a creature type" and
  -- is swapped; Elenda, Saint of Dusk's "hexproof from instants" comes back
  -- unchanged because its atom holds no subtype word. CR 702.11b's unqualified
  -- hexproof is the Nothing, which `fmap` leaves standing.
  Keyword.Type.Hexproof quality -> Keyword.Type.Hexproof (fmap (rewrite pairs) quality)
  -- CR 702.16a's "[quality]", which CR 612.2 asks the same question of: a
  -- protection quality naming a creature type is swapped, and Apostle of
  -- Purifying Light's colour comes back unchanged. Rule 702.16a always states a
  -- quality, so there is no Nothing to leave standing there; rule 702.16n's
  -- exception is the Nothing, which `fmap` leaves standing, and it is a word on
  -- the same card so CR 612.2 reaches it too.
  Keyword.Type.Protection protection ->
    Keyword.Type.Protection
      Protection.MkProtection
        { Protection.quality = rewrite pairs (Protection.quality protection),
          Protection.spares = fmap (rewrite pairs) (Protection.spares protection)
        }
  Keyword.Type.Deathtouch -> keyword
  Keyword.Type.Defender -> keyword
  Keyword.Type.DoubleStrike -> keyword
  Keyword.Type.FirstStrike -> keyword
  Keyword.Type.Flash -> keyword
  Keyword.Type.Banding -> keyword
  Keyword.Type.Flanking -> keyword
  Keyword.Type.Haunt -> keyword
  Keyword.Type.Phasing -> keyword
  Keyword.Type.Shadow -> keyword
  Keyword.Type.Horsemanship -> keyword
  Keyword.Type.Skulk -> keyword
  Keyword.Type.Melee -> keyword
  -- CR 702.124h and CR 702.124i name no colour, type or quality CR 612.2 could
  -- swap; CR 702.124k names only the Background enchantment type, and CR
  -- 702.124m only the Time Lord and Doctor creature types. None of these is a
  -- word this function carries, since none of these keywords holds a Filter.
  Keyword.Type.Partner -> keyword
  Keyword.Type.PartnerText _ -> keyword
  Keyword.Type.ChooseABackground -> keyword
  Keyword.Type.DoctorsCompanion -> keyword
  -- CR 702.124j's payload is a card NAME, which CR 612.2 says outright a
  -- subtype- or colour-word change cannot touch.
  Keyword.Type.PartnerWith _ -> keyword
  -- CR 702.23a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Rampage _ -> keyword
  Keyword.Type.Aftermath -> keyword
  Keyword.Type.JumpStart -> keyword
  -- CR 702.130a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Afflict _ -> keyword
  Keyword.Type.Flying -> keyword
  Keyword.Type.Haste -> keyword
  Keyword.Type.Indestructible -> keyword
  Keyword.Type.Lifelink -> keyword
  Keyword.Type.LivingMetal -> keyword
  Keyword.Type.Reach -> keyword
  Keyword.Type.Shroud -> keyword
  Keyword.Type.Trample -> keyword
  Keyword.Type.TrampleOverPlaneswalkers -> keyword
  Keyword.Type.Vigilance -> keyword
  -- CR 702.21a states its cost as part of the keyword too, so CR 612.1 reaches it
  -- the same way -- a ward cost naming a basic land type is the unprinted case
  -- the arms below are also a fence for.
  --
  -- CR 702.21b's perEach is left alone HERE and swapped at the mint instead
  -- (Pawl.Engine.Projection.mintedTriggeredAbilitiesOf, whose rewritePayGate
  -- takes the gate's own perEach), soulshift's posture: this module is below
  -- Pawl.Types.Quantity and has no quantity rewriter to call.
  Keyword.Type.Ward w -> Keyword.Type.Ward w {Ward.cost = rewriteCost pairs (Ward.cost w)}
  -- CR 702.33a, CR 702.33c, CR 702.34a, CR 702.37a and CR 702.42a: each states a
  -- cost as part of the keyword, so rewriteCost carries CR 612.1 into it.
  Keyword.Type.Kicker cost -> Keyword.Type.Kicker (rewriteCost pairs cost)
  Keyword.Type.Multikicker cost -> Keyword.Type.Multikicker (rewriteCost pairs cost)
  Keyword.Type.StickerKicker cost -> Keyword.Type.StickerKicker (rewriteCost pairs cost)
  Keyword.Type.Flashback cost -> Keyword.Type.Flashback (rewriteCost pairs cost)
  -- CR 702.162a states its cost as part of the keyword too.
  Keyword.Type.MoreThanMeetsTheEye cost -> Keyword.Type.MoreThanMeetsTheEye (rewriteCost pairs cost)
  -- CR 702.103a states its cost as part of the keyword too.
  Keyword.Type.Bestow cost -> Keyword.Type.Bestow (rewriteCost pairs cost)
  Keyword.Type.Mutate cost -> Keyword.Type.Mutate (rewriteCost pairs cost)
  Keyword.Type.Fear -> keyword
  Keyword.Type.Intimidate -> keyword
  Keyword.Type.Morph (Morph.MkMorph cost variant) -> Keyword.Type.Morph (Morph.MkMorph (rewriteCost pairs cost) variant)
  Keyword.Type.Entwine cost -> Keyword.Type.Entwine (rewriteCost pairs cost)
  -- CR 702.120a states its cost as part of the keyword too.
  Keyword.Type.Escalate cost -> Keyword.Type.Escalate (rewriteCost pairs cost)
  -- CR 702.27a states its cost as part of the keyword too.
  Keyword.Type.Buyback cost -> Keyword.Type.Buyback (rewriteCost pairs cost)
  -- CR 702.45a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Bushido _ -> keyword
  -- CR 702.46a's N is a number and not a word, so CR 612.2 has nothing to swap
  -- HERE. "Spirit" is a word CR 612.2a does reach, but it is in the ability
  -- Pawl.Engine.Keyword.soulshift mints rather than in this value, so the swap
  -- arrives there instead (Pawl.Engine.Projection.mintedTriggeredAbilitiesOf).
  Keyword.Type.Soulshift _ -> keyword
  Keyword.Type.Dredge _ -> keyword
  -- CR 702.104a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Tribute _ -> keyword
  -- CR 702.54a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "+1/+1 counter" is the rule's own noun and no card prints it.
  Keyword.Type.Bloodthirst _ -> keyword
  -- CR 702.61a names no word CR 612.2 can swap: "mana ability" is CR 605.1a's
  -- own classification and "the stack" is a zone.
  Keyword.Type.SplitSecond -> keyword
  -- CR 702.62a states a cost, so rewriteCost reaches it as flashback's does. The
  -- N is a number and not a word, and "time counter" is the rule's own noun.
  Keyword.Type.Suspend payload -> Keyword.Type.Suspend (fmap (\(Suspend.MkSuspend n cost) -> Suspend.MkSuspend n (rewriteCost pairs cost)) payload)
  -- CR 702.77a states a cost, so rewriteCost reaches it as flashback's does. The
  -- N is a number and not a word, and "+1/+1 counter" is in the ability
  -- Pawl.Engine.Keyword.reinforce mints rather than in this value.
  Keyword.Type.Reinforce (Reinforce.MkReinforce n cost) -> Keyword.Type.Reinforce (Reinforce.MkReinforce n (rewriteCost pairs cost))
  -- CR 702.43a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "+1/+1 counter" is the rule's own noun and no card prints it.
  Keyword.Type.Modular _ -> keyword
  Keyword.Type.Sunburst -> keyword
  -- CR 702.63a's N is a number and not a word -- and rule 702.63b's is absent
  -- altogether -- so CR 612.2 has nothing to swap here. "Time counter" is the
  -- rule's own noun; where a card does print it (Tidewalker) it rides that card's
  -- own entry rewrite and its CDA rather than this value.
  Keyword.Type.Vanishing _ -> keyword
  -- CR 702.32a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "fade counter" is in the replacement and the ability Pawl.Engine.Keyword mints
  -- rather than in this value.
  Keyword.Type.Fading _ -> keyword
  -- CR 702.68a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- the bonus is in the ability Pawl.Engine.Keyword.frenzy mints.
  Keyword.Type.Frenzy _ -> keyword
  -- CR 702.58a's and CR 702.64a's Ns are numbers too -- a count of counters and
  -- an amount of damage -- so CR 612.2 finds no word in either to swap.
  Keyword.Type.Graft _ -> keyword
  Keyword.Type.Absorb _ -> keyword
  Keyword.Type.Poisonous _ -> keyword
  -- CR 702.72a's [object] is a printed word, so CR 612.2 swaps inside it: under
  -- "Merfolk becomes Goblin" Wanderwine Prophets champions a Goblin, and the
  -- entry ability Pawl.Engine.Keyword.championEnters mints reads the rewritten
  -- quality.
  Keyword.Type.Champion quality -> Keyword.Type.Champion (rewrite pairs quality)
  Keyword.Type.Offering quality -> Keyword.Type.Offering (rewrite pairs quality)
  Keyword.Type.Renown _ -> keyword
  -- CR 702.85a is payload-free, so CR 612.2 has nothing to swap; the walk's
  -- nonland filter is in the ability Pawl.Engine.Keyword.cascade mints.
  Keyword.Type.Cascade -> keyword
  -- CR 702.60a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- the same-name filter its minted ability carries names no word either -- it
  -- reads the source's own name (Pawl.Engine.Keyword.ripple).
  Keyword.Type.Ripple _ -> keyword
  -- CR 702.40a is payload-free too.
  Keyword.Type.Storm -> keyword
  -- CR 702.69a is payload-free as well, and CR 702.78a's creatures are in the
  -- cost Pawl.Engine.Keyword.conspireCost mints.
  Keyword.Type.Gravestorm -> keyword
  Keyword.Type.Conspire -> keyword
  -- CR 702.153a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- the creature it names is in the cost Pawl.Engine.Keyword.casualtyCost mints.
  Keyword.Type.Casualty _ -> keyword
  -- CR 702.166a is payload-free and CR 702.194a's N is a number, so CR 612.2 has
  -- nothing to swap in either; the permanents they name are in the costs
  -- Pawl.Engine.Keyword.bargainCost and .teamworkCost mint.
  Keyword.Type.Bargain -> keyword
  Keyword.Type.Teamwork _ -> keyword
  -- CR 702.188a's and CR 702.190a's costs, flashback's shape: CR 612.2 swaps
  -- whatever a component of the PRINTED cost names. The creature each rule's own
  -- return names is not there to swap -- Pawl.Engine.Keyword.plainAlternativeCosts
  -- appends that component when the cost is offered, off the rule rather than off
  -- the card.
  Keyword.Type.WebSlinging cost -> Keyword.Type.WebSlinging (rewriteCost pairs cost)
  Keyword.Type.Sneak cost -> Keyword.Type.Sneak (rewriteCost pairs cost)
  -- CR 702.86a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Annihilator _ -> keyword
  -- CR 702.181a's and CR 702.189a's N is a number, or a count whose Filter the
  -- CARD writes (Avenger of the Fallen's creature cards), so CR 612.2 swaps
  -- whatever word that Filter names. Mobilize's "Warrior" is afterlife's
  -- "Spirit": a word CR 612.2a reaches in the ability Pawl.Engine.Keyword.mobilize
  -- mints rather than in this value, and firebending's {R} is a mana symbol.
  Keyword.Type.Mobilize n -> Keyword.Type.Mobilize (rewriteKeywordCount pairs n)
  Keyword.Type.Firebending n -> Keyword.Type.Firebending (rewriteKeywordCount pairs n)
  -- CR 702.75a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- the look and the exile are in the ability Pawl.Engine.Keyword.hideaway mints.
  Keyword.Type.Hideaway _ -> keyword
  Keyword.Type.Infect -> keyword
  Keyword.Type.Wither -> keyword
  -- CR 702.82c's [quality] is a Filter the CARD writes (Caprichrome's artifacts),
  -- so CR 612.2 swaps whatever word it names. Rule 702.82a's bare "creatures" is
  -- Nothing here and in the row Pawl.Engine.Keyword mints instead, and the count
  -- beside it is a number rather than a word.
  Keyword.Type.Devour devour -> Keyword.Type.Devour devour {Devour.quality = fmap (rewrite pairs) (Devour.quality devour)}
  -- CR 702.38a's payload is a count too, and its "share a creature type with
  -- it" names no word at all: the types are the entering object's own, which CR
  -- 612.2a reaches by changing that object rather than this keyword.
  Keyword.Type.Amplify _ -> keyword
  Keyword.Type.Exalted -> keyword
  -- CR 702.92a, 702.163a and 702.182a are nullary: their creature-type words
  -- (Germ, Rebel, Hero) are in the TOKEN Pawl.Engine.Keyword mints, which
  -- Pawl.Engine.Projection.Rewrite's Effect.Create arm rewrites as card data, so
  -- CR 612.2a reaches them there rather than here.
  Keyword.Type.LivingWeapon -> keyword
  Keyword.Type.ForMirrodin -> keyword
  Keyword.Type.JobSelect -> keyword
  -- CR 702.172a and 702.183a are nullary: what they name is the card's mode
  -- selection and CR 700.2h's per-mode costs, and a cost holds no word CR 612.2
  -- swaps.
  Keyword.Type.Spree -> keyword
  Keyword.Type.Tiered -> keyword
  Keyword.Type.Mentor -> keyword
  -- CR 702.135a's N is a number and not a word, so CR 612.2 has nothing to swap
  -- HERE. "Spirit" is a word CR 612.2a does reach, but it is in the ability
  -- Pawl.Engine.Keyword.afterlife mints rather than in this value, so the swap
  -- arrives there instead (Pawl.Engine.Projection.mintedTriggeredAbilitiesOf).
  Keyword.Type.Afterlife _ -> keyword
  Keyword.Type.Provoke -> keyword
  Keyword.Type.Training -> keyword
  Keyword.Type.BattleCry -> keyword
  Keyword.Type.Evolve -> keyword
  Keyword.Type.Exploit -> keyword
  Keyword.Type.Dethrone -> keyword
  Keyword.Type.Fuse -> keyword
  -- CR 702.6a's equip cost, level up's and outlast's shape, plus CR 702.6c's
  -- quality, which is a Filter and so descends the way cycling's does. CR
  -- 702.6e's target names no word CR 612.1 swaps; CR 702.67a's fortify cost is
  -- the cost half alone.
  Keyword.Type.Equip (Equip.MkEquip cost criterion onto) -> Keyword.Type.Equip (Equip.MkEquip (rewriteCost pairs cost) (fmap (rewrite pairs) criterion) onto)
  Keyword.Type.Fortify cost -> Keyword.Type.Fortify (rewriteCost pairs cost)
  Keyword.Type.AuraSwap cost -> Keyword.Type.AuraSwap (rewriteCost pairs cost)
  -- CR 702.49a's ninjutsu cost, fortify's shape: a Cost whose components can
  -- carry a Filter, and rule 702.49a's own return-an-unblocked-attacker component
  -- is minted rather than written, so nothing of the engine's descends here.
  Keyword.Type.Ninjutsu cost -> Keyword.Type.Ninjutsu (rewriteCost pairs cost)
  Keyword.Type.CumulativeUpkeep cost -> Keyword.Type.CumulativeUpkeep (rewriteCost pairs cost)
  Keyword.Type.Echo cost -> Keyword.Type.Echo (rewriteCost pairs cost)
  Keyword.Type.LevelUp cost -> Keyword.Type.LevelUp (rewriteCost pairs cost)
  Keyword.Type.Unearth cost -> Keyword.Type.Unearth (rewriteCost pairs cost)
  Keyword.Type.Embalm cost -> Keyword.Type.Embalm (rewriteCost pairs cost)
  Keyword.Type.Eternalize cost -> Keyword.Type.Eternalize (rewriteCost pairs cost)
  Keyword.Type.Outlast cost -> Keyword.Type.Outlast (rewriteCost pairs cost)
  Keyword.Type.Prowess -> keyword
  Keyword.Type.Extort -> keyword
  Keyword.Type.Increment -> keyword
  Keyword.Type.Menace -> keyword
  -- CR 702.73a names no word either: "every creature type" is CR 205.3m's
  -- whole family, so a CR 612.2 swap inside it has nothing to rewrite.
  Keyword.Type.Changeling -> keyword
  Keyword.Type.Devoid -> keyword
  -- CR 702.115a is payload-free, so it holds no word to swap: the library it
  -- names is "their" own, written into the rule rather than into the keyword.
  Keyword.Type.Ingest -> keyword
  -- CR 702.116a is payload-free too, and the seats its loop names are the
  -- rule's own words rather than the keyword's.
  Keyword.Type.Myriad -> keyword
  -- CR 702.122a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Crew _ -> keyword
  Keyword.Type.Saddle _ -> keyword
  Keyword.Type.Fabricate _ -> keyword
  Keyword.Type.Riot -> keyword
  Keyword.Type.Escape cost -> Keyword.Type.Escape (rewriteCost pairs cost)
  Keyword.Type.Evoke cost -> Keyword.Type.Evoke (rewriteCost pairs cost)
  Keyword.Type.Dash cost -> Keyword.Type.Dash (rewriteCost pairs cost)
  Keyword.Type.Blitz cost -> Keyword.Type.Blitz (rewriteCost pairs cost)
  Keyword.Type.Reconfigure cost -> Keyword.Type.Reconfigure (rewriteCost pairs cost)
  Keyword.Type.Warp cost -> Keyword.Type.Warp (rewriteCost pairs cost)
  Keyword.Type.Disturb cost -> Keyword.Type.Disturb (rewriteCost pairs cost)
  Keyword.Type.Harmonize cost -> Keyword.Type.Harmonize (rewriteCost pairs cost)
  Keyword.Type.Cleave cost -> Keyword.Type.Cleave (rewriteCost pairs cost)
  Keyword.Type.Overload cost -> Keyword.Type.Overload (rewriteCost pairs cost)
  Keyword.Type.Awaken cost -> Keyword.Type.Awaken (rewriteCost pairs cost)
  -- CR 702.119b's quality rides the payload beside the cost, so both halves
  -- take rule 612.2's swap.
  Keyword.Type.Emerge (Emerge.MkEmerge cost criterion) -> Keyword.Type.Emerge (Emerge.MkEmerge (rewriteCost pairs cost) (fmap (rewrite pairs) criterion))
  Keyword.Type.Surge cost -> Keyword.Type.Surge (rewriteCost pairs cost)
  Keyword.Type.Spectacle cost -> Keyword.Type.Spectacle (rewriteCost pairs cost)
  Keyword.Type.Prowl cost -> Keyword.Type.Prowl (rewriteCost pairs cost)
  Keyword.Type.Freerunning cost -> Keyword.Type.Freerunning (rewriteCost pairs cost)
  Keyword.Type.Impending (Impending.MkImpending n cost) -> Keyword.Type.Impending (Impending.MkImpending n (rewriteCost pairs cost))
  Keyword.Type.Unleash -> keyword
  -- CR 702.150a names no word CR 612.2 can swap: it is written about loyalty
  -- counters and Phyrexian mana symbols, both the rules' own vocabulary.
  Keyword.Type.Compleated -> keyword
  Keyword.Type.ReadAhead -> keyword
  Keyword.Type.Demonstrate -> keyword
  Keyword.Type.Daybound -> keyword
  Keyword.Type.Nightbound -> keyword
  -- CR 702.147a names no word CR 612.2 can swap: "end of combat" is the rules'
  -- own step and the ability it arms is written in Pawl.Engine.Keyword.
  Keyword.Type.Decayed -> keyword
  -- CR 718.1's inset frame is a mana cost and a printed box: no word for CR
  -- 612.1 to change.
  Keyword.Type.Prototype _ -> keyword
  Keyword.Type.Toxic _ -> keyword
  -- CR 702.165a names no word CR 612.2 can swap: the count is a number, and
  -- printedAbove is compared against the COPIABLE keywords (CR 707.2), which no
  -- text change reaches, so it is left as written.
  Keyword.Type.Backup _ -> keyword
  -- CR 702.168a states a cost, so rewriteCost reaches it as morph's does. The
  -- ward {2} rule 702.168b lists is NOT reached from here: that keyword is on the
  -- face-down object's own characteristics (Pawl.Types.FaceDownCharacteristics),
  -- and a text-changing effect that reached this ability would find no word in it
  -- to swap either.
  Keyword.Type.Disguise cost -> Keyword.Type.Disguise (rewriteCost pairs cost)
  -- CR 702.170a states a cost, so rewriteCost reaches it as flashback's does.
  Keyword.Type.Plot cost -> Keyword.Type.Plot (rewriteCost pairs cost)
  -- CR 702.56a states a cost, so rewriteCost reaches it as flashback's does.
  Keyword.Type.Replicate cost -> Keyword.Type.Replicate (rewriteCost pairs cost)
  Keyword.Type.Recover cost -> Keyword.Type.Recover (rewriteCost pairs cost)
  Keyword.Type.Ravenous -> keyword
  Keyword.Type.Squad cost -> Keyword.Type.Squad (rewriteCost pairs cost)
  Keyword.Type.Offspring cost -> Keyword.Type.Offspring (rewriteCost pairs cost)
  -- CR 702.174a's payload is a [something] and not a Cost, so a CR 612.2 land-type
  -- change has nothing in it to rewrite.
  Keyword.Type.Gift _ -> keyword
  -- CR 702.143a states a cost too, so it is reached the same way. A cost read
  -- off the card's own mana cost prints no word to swap.
  Keyword.Type.Foretell (ForetellCost.Stated cost) -> Keyword.Type.Foretell (ForetellCost.Stated (rewriteCost pairs cost))
  Keyword.Type.Foretell (ForetellCost.ManaCostReducedBy _) -> keyword
  -- CR 702.139a states a CONDITION rather than a cost, so `rewrite` reaches it
  -- as landwalk's criterion is reached. No text-changing effect can be in play
  -- when it is read (CR 103.2b runs before the game begins), so this descent is
  -- structural rather than something a board exercises.
  Keyword.Type.Companion condition -> Keyword.Type.Companion (rewrite pairs condition)
  -- CR 702.94a states a cost too, so it is reached the same way.
  Keyword.Type.Miracle cost -> Keyword.Type.Miracle (rewriteCost pairs cost)
  Keyword.Type.Ascend -> keyword
  Keyword.Type.Storied -> keyword
  -- CR 702.132a names no quality and carries no cost: the mana it speaks about
  -- is the spell's own total cost, so CR 612.2 has nothing here to swap.
  Keyword.Type.Assist -> keyword
  -- CR 702.177a names no quality and carries no cost: what it adds is a rider on
  -- the ability printed after it, so CR 612.2 has nothing here to swap.
  Keyword.Type.Exhaust -> keyword
  -- CR 702.142a, exhaust's reason one rule over: what boast adds is a rider on
  -- the ability printed after it, so CR 612.2 has nothing here to swap.
  Keyword.Type.Boast -> keyword
  Keyword.Type.Forecast -> keyword
  Keyword.Type.PowerUp -> keyword
  -- CR 716.2 names no quality: the level is a number, so CR 612.2 has nothing
  -- here to swap.
  Keyword.Type.ClassLevel _ -> keyword
  Keyword.Type.StartYourEngines -> keyword
  -- CR 701.43d names no quality and carries no cost, so CR 612.2 has nothing here
  -- to swap.
  Keyword.Type.Exert -> keyword
  Keyword.Type.Enlist -> keyword
  Keyword.Type.Persist -> keyword
  Keyword.Type.Undying -> keyword
  Keyword.Type.Soulbond -> keyword
  -- CR 702.184a's ability names "creature" and "charge counters", both the rules'
  -- own vocabulary; the criterion is written in Pawl.Engine.Keyword rather than
  -- on the card, so CR 612.2 has no printed word here to swap.
  Keyword.Type.Station -> keyword
  Keyword.Type.UmbraArmor -> keyword
  -- CR 702.51a and CR 702.126a name "creature", "artifact" and a mana symbol's
  -- own color, and CR 702.66a names no quality at all -- all the rules' own
  -- vocabulary, and the criteria they produce are written in Pawl.Engine.Keyword
  -- rather than on the card, so CR 612.2 has no printed word here to swap.
  Keyword.Type.Epic -> keyword
  Keyword.Type.Paradigm -> keyword
  Keyword.Type.Cipher -> keyword
  Keyword.Type.Convoke -> keyword
  Keyword.Type.Delve -> keyword
  Keyword.Type.Improvise -> keyword
  -- CR 702.41a's [text] is a printed quality, so `rewrite` reaches it as
  -- landwalk's criterion is reached. Affinity's reader,
  -- Pawl.Engine.Cost.selfReductions, takes the projected keywords, so the
  -- rewritten quality is the one counted.
  Keyword.Type.Affinity quality -> Keyword.Type.Affinity (rewrite pairs quality)
  -- CR 702.125a counts OPPONENTS, so there is no printed word here to swap.
  Keyword.Type.Undaunted -> keyword
  -- CR 702.81a names "a land card", the rules' own vocabulary, and the criterion
  -- is written in Pawl.Engine.Cost rather than on the card -- so CR 612.2 has no
  -- printed word here to swap.
  Keyword.Type.Retrace -> keyword
  -- CR 702.187b's cost, where there is one, is printed, so its components take the same descent
  -- flashback's do.
  Keyword.Type.Mayhem cost -> Keyword.Type.Mayhem (fmap (rewriteCost pairs) cost)
  -- CR 702.35a's cost, where it is stated, is printed, so its components take
  -- the same descent mayhem's do; the card's own mana cost names no word.
  Keyword.Type.Madness (MadnessCost.Stated cost) -> Keyword.Type.Madness (MadnessCost.Stated (rewriteCost pairs cost))
  Keyword.Type.Madness MadnessCost.OwnManaCost -> keyword
  -- CR 702.88a takes no parameter and prints no cost, retrace's position: rule
  -- 702.88a's exile, upkeep and free cast are all the rule's, so CR 612.2 has no
  -- printed word here to swap.
  Keyword.Type.Rebound -> keyword
  -- CR 702.97a's and CR 702.141a's costs are printed, so their components take
  -- the same descent embalm's does.
  Keyword.Type.Scavenge cost -> Keyword.Type.Scavenge (rewriteCost pairs cost)
  Keyword.Type.Encore cost -> Keyword.Type.Encore (rewriteCost pairs cost)
  Keyword.Type.Transmute cost -> Keyword.Type.Transmute (rewriteCost pairs cost)
  Keyword.Type.Transfigure cost -> Keyword.Type.Transfigure (rewriteCost pairs cost)
  -- CR 702.167a's payload is not a bare Cost: its cost and
  -- its [materials] criterion are both printed, so both halves descend.
  Keyword.Type.Craft crafting ->
    let materials = Craft.materials crafting
        swapped = ExileMaterials.MkExileMaterials (ExileMaterials.count materials) (ExileMaterials.orMore materials) (rewrite pairs (ExileMaterials.whichObjects materials))
     in Keyword.Type.Craft (Craft.MkCraft (rewriteCost pairs (Craft.cost crafting)) swapped)
  -- CR 702.47a's [quality] and [cost] are both printed, so both halves descend,
  -- craft's reason.
  Keyword.Type.Splice splicing -> Keyword.Type.Splice (Splice.MkSplice (rewrite pairs (Splice.onto splicing)) (rewriteCost pairs (Splice.cost splicing)))

-- rewriteKeyword's descent into mobilize's and firebending's N: only a tally
-- carries a Filter, and its scope holds no word, as rewriteQuantity leaves
-- Count's.
rewriteKeywordCount :: [(Subtype.Subtype, Subtype.Subtype)] -> KeywordCount.KeywordCount Keyword.Type.Keyword -> KeywordCount.KeywordCount Keyword.Type.Keyword
rewriteKeywordCount pairs n = case n of
  KeywordCount.Tally tally -> KeywordCount.Tally tally {KeywordTally.filter = rewrite pairs (KeywordTally.filter tally)}
  KeywordCount.Fixed _ -> n
  KeywordCount.Power -> n
  KeywordCount.PlayerCounters _ -> n

-- CR 612.1's word swap inside a COST. CR 118.1 makes a cost "an action or payment
-- necessary to take another action", and the one on an activated ability is
-- printed in the text box that rule 612 reaches -- Dark Heart of the Wood's
-- "Sacrifice a Forest:" is the printing, and a Magical Hack naming Forest turns it
-- into "Sacrifice an Island:". CR 602.2a is why fixing it here is enough for the
-- payment: the ability on the stack "has the text of the ability that created
-- it", so it pays the cost this rewrite produced.
--
-- A CR 118.12 cost offered as a clause resolves is printed in that same text box
-- and takes the same descent -- Lithophage's "unless you sacrifice a Mountain",
-- through Pawl.Engine.Projection.Rewrite.rewritePayGate.
--
-- Here rather than in Pawl.Engine.Projection beside the ability rewriters, because
-- rewriteKeyword above needs it too and Pawl.Engine.Filter cannot import
-- Pawl.Engine.Projection. One descent, so the keyword carrier and the ability
-- carrier cannot drift apart.
--
-- The MANA part is left alone and that is CR 612.2 rather than an omission: a mana
-- symbol is a symbol and not a land type word, and "a land type word used as a
-- land type" is the only use of these pairs the rule licenses.
--
-- Exhaustive rather than a catch-all, rewrite's and rewriteKeyword's stated
-- posture: a later component that can carry a Filter must fail to compile here
-- instead of silently keeping the printed word.
rewriteCost :: [(Subtype.Subtype, Subtype.Subtype)] -> Cost.Cost Keyword.Type.Keyword -> Cost.Cost Keyword.Type.Keyword
rewriteCost pairs cost = cost {Cost.components = fmap (rewriteComponent pairs) (Cost.components cost)}

-- rewriteCounterKind over the kind a counter removal names, where it names one.
rewriteWhichCounters :: [(Subtype.Subtype, Subtype.Subtype)] -> WhichCounters.WhichCounters Keyword.Type.Keyword -> WhichCounters.WhichCounters Keyword.Type.Keyword
rewriteWhichCounters pairs which = case which of
  WhichCounters.OfKind kind -> WhichCounters.OfKind (rewriteCounterKind pairs kind)
  WhichCounters.OfAnyKind -> which

-- rewriteCost's per-component half. The components that carry a Filter are the
-- ones that descend; the rest name a number, or the object the cost is on, and
-- CR 612.2 finds no word in them to swap.
--
-- Of those, only Sacrifice has a producer: Dark Heart of the Wood on an
-- activation cost, and Lithophage on the cost a trigger offers as it resolves
-- (CR 118.12). The TapForTotalPower, TapPermanents, DiscardCards,
-- ExileCardsFromGraveyard, ExileTopFromGraveyard, ReturnPermanents,
-- ExileCardFromHand, RevealCardFromHand, Behold, BeholdAndExile, ExileMaterials,
-- RemoveCounters and
-- PutCardFromHandOntoBattlefield arms
-- are a regression
-- fence: no printing pairs any of them with a basic land type, so no test can
-- falsify them. Magmatic
-- Insight's "discard a land card" and Hakbal of the Surging Soul's "a land card
-- from your hand" come closest and are still not one -- CR 612.2 swaps a SUBTYPE
-- word, and the land CARD TYPE is not one.
rewriteComponent :: [(Subtype.Subtype, Subtype.Subtype)] -> CostComponent.CostComponent Keyword.Type.Keyword -> CostComponent.CostComponent Keyword.Type.Keyword
rewriteComponent pairs component = case component of
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) -> CostComponent.Sacrifice (Sacrifice.MkSacrifice n (rewrite pairs criterion))
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) -> CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n (rewrite pairs criterion))
  CostComponent.TapPermanents (TapPermanents.MkTapPermanents n criterion sharing) -> CostComponent.TapPermanents (TapPermanents.MkTapPermanents n (rewrite pairs criterion) sharing)
  CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n criterion) -> CostComponent.ReturnPermanents (ReturnPermanents.MkReturnPermanents n (rewrite pairs criterion))
  CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n criterion) -> CostComponent.ExilePermanents (ExilePermanents.MkExilePermanents n (rewrite pairs criterion))
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) -> CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n (rewrite pairs criterion))
  CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore criterion) -> CostComponent.ExileMaterials (ExileMaterials.MkExileMaterials n orMore (rewrite pairs criterion))
  CostComponent.ExileTopFromGraveyard criterion -> CostComponent.ExileTopFromGraveyard (rewrite pairs criterion)
  -- Untouched: CR 701.59a describes the cards by a total and states no card type,
  -- so rule 612.1 finds no word in this component to swap.
  CostComponent.CollectEvidence _ -> component
  CostComponent.CollectEvidenceOfTargets -> component
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) -> CostComponent.DiscardCards (DiscardCards.MkDiscardCards n (rewrite pairs criterion))
  CostComponent.PutCardFromHandOntoBattlefield criterion -> CostComponent.PutCardFromHandOntoBattlefield (rewrite pairs criterion)
  CostComponent.ExileCardFromHand criterion -> CostComponent.ExileCardFromHand (rewrite pairs criterion)
  CostComponent.RevealCardFromHand criterion -> CostComponent.RevealCardFromHand (rewrite pairs criterion)
  CostComponent.Behold (Behold.MkBehold n criterion) -> CostComponent.Behold (Behold.MkBehold n (rewrite pairs criterion))
  CostComponent.BeholdAndExile criterion -> CostComponent.BeholdAndExile (rewrite pairs criterion)
  CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents n which criterion spread) -> CostComponent.RemoveCounters (CountersFromPermanents.MkCountersFromPermanents n (rewriteWhichCounters pairs which) (rewrite pairs criterion) spread)
  CostComponent.TapThis -> component
  CostComponent.UntapThis -> component
  CostComponent.SacrificeThis -> component
  CostComponent.ReturnThis -> component
  CostComponent.PayLife _ -> component
  CostComponent.PayHalfLife _ -> component
  CostComponent.DiscardThis _ -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.RemoveCountersFromThis _ -> component
  CostComponent.PutPlusOneCountersOnThis _ -> component
  CostComponent.Blight _ -> component
  CostComponent.Forage -> component
  CostComponent.FlipCoin -> component
  CostComponent.ExileThisFromGraveyard -> component
  CostComponent.ExileThis -> component
  CostComponent.MillCards _ -> component
  CostComponent.RevealTopOfLibrary _ -> component
  CostComponent.ChooseOpponent -> component
  CostComponent.Waterbend _ -> component
  CostComponent.WaterbendInstead _ -> component

-- CR 603.2: replace every ControlledByBound atom whose slot this environment
-- names with the baked ControlledByPlayer arm. What makes "target creature THAT
-- PLAYER controls" answerable at all -- the player is a fact about the EVENT that
-- fired the trigger, and the two sites that judge a target slot
-- (Pawl.Engine.Engine.placeBorne at CR 603.3d, Pawl.Engine.Resolve.resolveModes
-- at CR 608.2b) are the ones holding it, so they substitute before the match
-- rather than a Context field carrying the bindings into every match that will
-- never ask.
--
-- The atom is LEFT STANDING when the slot names no one player, which is the
-- honest answer rather than a defensive one: `matches` reads it as False, so a
-- slot nothing bound admits no candidate, exactly as an absent perspective makes
-- ControlledBy vacuous.
--
-- NO DESCENT INTO A KEYWORD, unlike `rewrite` above, and the difference is
-- load-bearing: HasKeyword compares its keyword against the PROJECTION's keyword
-- map by equality, so baking a player into a Filter one carries would produce a
-- key the projection can never hold. There is no reading of a card under which a
-- keyword's own criterion names a trigger's player anyway -- CR 702.14c's
-- criterion is a land description.
--
-- Exhaustive rather than a catch-all, `rewrite`'s posture: a later atom that can
-- carry a Filter must fail to compile here instead of silently keeping an
-- unbaked one.
bakeBound :: Map.Map SlotName.SlotName PlayerId.PlayerId -> Filter.Filter Keyword.Type.Keyword -> Filter.Filter Keyword.Type.Keyword
bakeBound players predicate = case predicate of
  Filter.ControlledByBound slot -> maybe predicate Filter.ControlledByPlayer (Map.lookup slot players)
  Filter.And fs -> Filter.And (fmap (bakeBound players) fs)
  Filter.Or fs -> Filter.Or (fmap (bakeBound players) fs)
  Filter.Not f -> Filter.Not (bakeBound players f)
  -- Descended into for the reason `rewrite` descends: the nested filter is a
  -- filter like any other, and a slot named inside it must be baked before the
  -- match or it can never be answered.
  Filter.ControlsMoreThanYou n f -> Filter.ControlsMoreThanYou n (bakeBound players f)
  Filter.ControlledByPlayer _ -> predicate
  -- Untouched: CR 603.2's slot is not the recipient an effect has reached, and no
  -- binding could answer this atom -- Pawl.Engine.Filter.Context carries it.
  Filter.ControlledByRecipient -> predicate
  Filter.HasCardType _ -> predicate
  Filter.HasSupertype _ -> predicate
  Filter.HasColor _ -> predicate
  Filter.IsMonocolored -> predicate
  Filter.SharesColorWithSource -> predicate
  Filter.HasSubtype _ -> predicate
  Filter.HasName _ -> predicate
  Filter.NameWordsAtLeast _ -> predicate
  Filter.HasNameOriginallyPrintedIn _ -> predicate
  Filter.HasKeyword _ -> predicate
  Filter.HasKeywordFamily _ -> predicate
  -- Untouched: CR 603.2's map holds PLAYERS, and a slot this atom's operand
  -- names holds a number or an object. It stays answerable where it is written,
  -- the Context carrying the number into the match rather than a substitution
  -- making it.
  Filter.Measures _ -> predicate
  Filter.ManaValueIsEven -> predicate
  Filter.ControlledBy _ -> predicate
  Filter.ControlledByDefendingPlayer -> predicate
  Filter.OwnedBy _ -> predicate
  Filter.OwnedByRecipient -> predicate
  Filter.IsSource -> predicate
  Filter.IsObject _ -> predicate
  -- Untouched for IsSource's reason: both read the Context, and neither names a
  -- slot.
  Filter.TargetsSource -> predicate
  Filter.TargetsOnlySource -> predicate
  Filter.HasSingleTarget -> predicate
  -- DESCENDED into for the reason AttachedTo below is: the nest is a description
  -- of another object and may name a bound slot, which this function's pairing
  -- with overBoundSlots requires be baked here and reported there.
  Filter.TargetsOnlyOne f -> Filter.TargetsOnlyOne (bakeBound players f)
  Filter.TargetsMatching f -> Filter.TargetsMatching (bakeBound players f)
  Filter.TargetsPlayer _ -> predicate
  -- Untouched for the reason IsControllerOfBound below is, and one step shorter:
  -- CR 603.2's binding map holds PLAYERS and this atom names a slot holding an
  -- OBJECT. Pawl.Engine.Filter.matches answers it as it stands.
  Filter.IsBound _ -> predicate
  Filter.IsTarget -> predicate
  Filter.SameNameAsBound _ -> predicate
  Filter.SameNameAsSource -> predicate
  Filter.SameOwnerAsSource -> predicate
  Filter.SameControllerAsBound _ -> predicate
  Filter.SameControllerAsHostOfBound _ -> predicate
  Filter.SharesCreatureTypeWithBound _ -> predicate
  Filter.HasChosenName -> predicate
  Filter.HasChosenColor -> predicate
  Filter.HasChosenSubtype -> predicate
  Filter.IsLastExiledWithSource -> predicate
  Filter.OfChosenPlayer -> predicate
  Filter.OfRelatedPlayer _ -> predicate
  Filter.IsPlayer _ -> predicate
  -- Untouched: CR 603.2's binding map holds PLAYERS, and this atom names a slot
  -- holding an OBJECT -- there is nothing here to substitute.
  -- Pawl.Engine.Count.bakePerspective is where it is answered instead.
  Filter.IsControllerOfBound _ -> predicate
  -- Untouched: the atom names no slot at all, so CR 603.2's map has nothing to
  -- substitute into it. Baked one module out, as the atom above is.
  Filter.CardsInGraveyardAtLeast _ -> predicate
  Filter.IsAttacking -> predicate
  Filter.IsAttackingPlayer _ -> predicate
  Filter.IsAttackingPlaneswalker _ -> predicate
  Filter.IsAttackingBattle _ -> predicate
  Filter.IsBlocking -> predicate
  Filter.IsBlocked -> predicate
  Filter.AttackedThisTurn -> predicate
  Filter.DeclaredAttackerThisCombat -> predicate
  Filter.DeclaredAttackedThisCombat -> predicate
  Filter.DeclaredBlockerThisCombat -> predicate
  Filter.MilledThisTurn -> predicate
  Filter.CantCrewVehicles -> predicate
  Filter.DealtDamageThisTurn -> predicate
  Filter.EnteredThisTurn -> predicate
  Filter.CrewedSourceThisTurn -> predicate
  Filter.ConvokedSourceThisTurn -> predicate
  Filter.SaddledSourceThisTurn -> predicate
  -- Untouched: the atom names no slot for CR 603.2's map to substitute into.
  Filter.ControlledSinceTurnBegan -> predicate
  -- DESCENT, for ControlsMoreThanYou's reason above: a ControlledByBound written
  -- into the HOST's description is baked exactly as the same atom written at the
  -- top level would be. Pawl.Engine.Filter.boundSlots descends to match, which is
  -- the pairing that function's comment insists on.
  Filter.AttachedTo f -> Filter.AttachedTo (bakeBound players f)
  -- DESCENT, for the atom above's reason: a ControlledByBound written into the
  -- represented card's description is baked exactly as the same atom at the top
  -- level would be, and Pawl.Engine.Filter.boundSlots descends to match.
  Filter.RepresentedByCard f -> Filter.RepresentedByCard (bakeBound players f)
  -- DESCENT, for the atom above's reason: a ControlledByBound written into the
  -- ATTACHER's description is baked exactly as the same atom at the top level
  -- would be, and Pawl.Engine.Filter.boundSlots descends to match.
  Filter.HasAttached f -> Filter.HasAttached (bakeBound players f)
  Filter.IsAttachedToSource -> predicate
  Filter.IsAttachedToEvaluated -> predicate
  Filter.IsHostOfSource -> predicate
  Filter.EnteredWithSource -> predicate
  Filter.AttachedNoLaterThanSource -> predicate
  Filter.CanHostSubject -> predicate
  Filter.CanAttachToSubject -> predicate
  Filter.HostOfSubjectHasCardType _ -> predicate
  Filter.IsCommander -> predicate
  Filter.IsToken -> predicate
  Filter.IsActivatedAbility -> predicate
  Filter.IsAbility -> predicate
  Filter.IsEmblem -> predicate
  -- Descended into, HasAttached's reason: the SOURCE's description is baked
  -- exactly as the same atom at the top level would be, and boundSlots descends
  -- to match.
  Filter.FromSource f -> Filter.FromSource (bakeBound players f)
  Filter.IsTapped -> predicate
  Filter.IsFaceDown -> predicate
  Filter.IsExiledFaceDown -> predicate
  Filter.Transformed -> predicate
  Filter.IsRingBearer -> predicate
  Filter.IsPaired -> predicate
  Filter.IsPairedWithSource -> predicate
  Filter.IsBlockedBySource -> predicate
  Filter.HasDesignation _ -> predicate
  Filter.HasCounters _ -> predicate
  Filter.HasCountersOfAnyKind -> predicate
  Filter.HasSticker _ -> predicate
  Filter.Stickered -> predicate
  Filter.HasNonManaActivatedAbility -> predicate
  Filter.HasActivatedAbility -> predicate
  Filter.IsInZone _ -> predicate
  Filter.WasCastFrom _ -> predicate
  Filter.TagWasSpent _ -> predicate
  Filter.Kicked -> predicate

-- The mana-value LITERALS a Filter compares against: every `n` a Measures atom
-- compares the candidate's mana value with, at any depth.
--
-- CR 601.3a's lookahead is the one caller (Pawl.Engine.PlayerEffect
-- prohibitsCasting). Asking whether some choice of X could take a spell out of a
-- prohibition means asking one Filter at more than one mana value, and this is
-- what BOUNDS that search: a literal comparison and ManaValueIsEven are the
-- whole of the mana-value vocabulary a filter in this POSITION may use, so above
-- every literal returned here the only distinction a Filter can still draw is
-- parity, and a sample running two past the greatest literal has already seen
-- every verdict the Filter can give. A comparison against any other operand reads
-- a mana value too, and the position is why none widens the sample -- see the
-- Measures arm below.
--
-- Exhaustive rather than a catch-all, bakeBound's posture and for a sharper
-- reason: an atom reading the mana value some other way -- a multiple-of-three
-- test -- would break that argument, so it must break this build instead of
-- silently narrowing the search.
--
-- Polymorphic in the keyword, since no arm reads one.
manaValueThresholds :: Filter.Filter keyword -> [Integer]
manaValueThresholds predicate = case predicate of
  -- A literal against the candidate's mana value, whichever way it compares:
  -- above every such literal the comparison's answer is constant, so the sample
  -- has already seen it.
  Filter.Measures m -> case Measures.operand m of
    Operand.Literal n -> case Measures.measure m of
      Measure.ManaValue -> [n]
      -- Reads power or toughness against the literal, so it bounds nothing.
      Measure.Power -> []
      Measure.Toughness -> []
    -- No literal to report, whatever the measure -- and an Own ManaValue
    -- operand reads the candidate's mana value from the other side. Position is
    -- what keeps the caller's argument whole: CR 601.3a's lookahead reads a
    -- player ability's prohibition filter
    -- (Pawl.Engine.PlayerEffect.prohibitsCasting), and
    -- Pawl.FilterPositionLintSpec's "CR 601.3a no player effect compares a mana
    -- value against anything but a literal" keeps every read of the candidate's
    -- mana value through any other operand, on either side, out of that position.
    Operand.OfSource _ -> []
    Operand.Own _ -> []
    Operand.OfBound _ -> []
    Operand.AmountInSlot _ -> []
    Operand.EnclosingAmount -> []
  Filter.And fs -> concatMap manaValueThresholds fs
  Filter.Or fs -> concatMap manaValueThresholds fs
  Filter.Not f -> manaValueThresholds f
  -- Descended into, which OVER-reports: the literals inside bound the mana value
  -- of the permanents being counted, never the candidate's own. Reporting them
  -- only widens CR 601.3a's sample, and the alternative -- an empty list -- would
  -- have to argue that no nested atom can ever matter, which is a claim about the
  -- inner filter rather than about this atom.
  Filter.ControlsMoreThanYou _ f -> manaValueThresholds f
  -- Reads the mana value and compares it against NO literal, so it bounds
  -- nothing: parity is what the sample's two-past-the-greatest tail is for.
  Filter.ManaValueIsEven -> []
  Filter.HasCardType _ -> []
  Filter.HasSupertype _ -> []
  Filter.HasColor _ -> []
  Filter.IsMonocolored -> []
  Filter.SharesColorWithSource -> []
  Filter.HasSubtype _ -> []
  Filter.HasName _ -> []
  Filter.NameWordsAtLeast _ -> []
  Filter.HasNameOriginallyPrintedIn _ -> []
  Filter.HasKeyword _ -> []
  Filter.HasKeywordFamily _ -> []
  Filter.ControlledBy _ -> []
  Filter.ControlledByDefendingPlayer -> []
  Filter.ControlledByBound _ -> []
  Filter.ControlledByPlayer _ -> []
  Filter.ControlledByRecipient -> []
  Filter.OwnedBy _ -> []
  Filter.OwnedByRecipient -> []
  Filter.IsSource -> []
  Filter.IsObject _ -> []
  Filter.TargetsSource -> []
  Filter.TargetsOnlySource -> []
  Filter.HasSingleTarget -> []
  -- Descended into for AttachedTo's reason: the nest is a description of another
  -- object and may carry a mana-value bound of its own.
  Filter.TargetsOnlyOne f -> manaValueThresholds f
  Filter.TargetsMatching f -> manaValueThresholds f
  Filter.TargetsPlayer _ -> []
  Filter.IsBound _ -> []
  Filter.IsTarget -> []
  Filter.SameNameAsBound _ -> []
  Filter.SameNameAsSource -> []
  Filter.SameOwnerAsSource -> []
  Filter.SameControllerAsBound _ -> []
  Filter.SameControllerAsHostOfBound _ -> []
  Filter.SharesCreatureTypeWithBound _ -> []
  Filter.HasChosenName -> []
  Filter.HasChosenColor -> []
  Filter.HasChosenSubtype -> []
  Filter.IsLastExiledWithSource -> []
  Filter.OfChosenPlayer -> []
  Filter.OfRelatedPlayer _ -> []
  Filter.IsPlayer _ -> []
  Filter.IsControllerOfBound _ -> []
  -- No threshold: the literal bounds a COUNT OF CARDS in a zone, not a mana
  -- value, so CR 601.3a's sample has nothing to learn from it.
  Filter.CardsInGraveyardAtLeast _ -> []
  Filter.IsAttacking -> []
  Filter.IsAttackingPlayer _ -> []
  Filter.IsAttackingPlaneswalker _ -> []
  Filter.IsAttackingBattle _ -> []
  Filter.IsBlocking -> []
  Filter.IsBlocked -> []
  Filter.AttackedThisTurn -> []
  Filter.DeclaredAttackerThisCombat -> []
  Filter.DeclaredAttackedThisCombat -> []
  Filter.DeclaredBlockerThisCombat -> []
  Filter.MilledThisTurn -> []
  Filter.CantCrewVehicles -> []
  Filter.DealtDamageThisTurn -> []
  Filter.EnteredThisTurn -> []
  Filter.CrewedSourceThisTurn -> []
  Filter.ConvokedSourceThisTurn -> []
  Filter.SaddledSourceThisTurn -> []
  Filter.ControlledSinceTurnBegan -> []
  -- Descended into, which OVER-reports for ControlsMoreThanYou's reason: the
  -- literals inside bound the HOST's mana value and never the candidate's. Only
  -- widening CR 601.3a's sample is the safe direction.
  Filter.AttachedTo f -> manaValueThresholds f
  -- Descended into, and OVER-reporting for the atom above's reason: the literals
  -- inside bound the represented CARD's mana value and never the candidate's.
  -- Only widening CR 601.3a's sample is the safe direction.
  Filter.RepresentedByCard f -> manaValueThresholds f
  -- Descended into, and OVER-reporting for the atom above's reason: the literals
  -- inside bound an ATTACHER's mana value and never the candidate's. Only
  -- widening CR 601.3a's sample is the safe direction.
  Filter.HasAttached f -> manaValueThresholds f
  Filter.IsAttachedToSource -> []
  Filter.IsAttachedToEvaluated -> []
  Filter.IsHostOfSource -> []
  Filter.EnteredWithSource -> []
  Filter.AttachedNoLaterThanSource -> []
  Filter.CanHostSubject -> []
  Filter.CanAttachToSubject -> []
  Filter.HostOfSubjectHasCardType _ -> []
  Filter.IsCommander -> []
  Filter.IsToken -> []
  Filter.IsActivatedAbility -> []
  Filter.IsAbility -> []
  Filter.IsEmblem -> []
  Filter.FromSource f -> manaValueThresholds f
  Filter.IsTapped -> []
  Filter.IsFaceDown -> []
  Filter.IsExiledFaceDown -> []
  Filter.Transformed -> []
  Filter.IsRingBearer -> []
  Filter.IsPaired -> []
  Filter.IsPairedWithSource -> []
  Filter.IsBlockedBySource -> []
  Filter.HasDesignation _ -> []
  Filter.HasCounters _ -> []
  Filter.HasCountersOfAnyKind -> []
  Filter.HasSticker _ -> []
  Filter.Stickered -> []
  Filter.HasNonManaActivatedAbility -> []
  Filter.HasActivatedAbility -> []
  Filter.IsInZone _ -> []
  Filter.WasCastFrom _ -> []
  Filter.TagWasSpent _ -> []
  Filter.Kicked -> []

-- CR 701.23b vs CR 701.23d: does this predicate state a QUALITY? A search whose
-- filter states one may find fewer cards than it asks for, or none, even when the
-- zone holds them (701.23b); a search "simply for a quantity of cards" -- "a
-- card", which is the whole of Extract's filter -- must find that many if the
-- zone can supply them (701.23d). Pawl.Engine.Resolve's Search arm is one
-- caller, and there the answer is the difference between an answer of "nothing"
-- being honoured and being overridden. Pawl.Engine.Cost.statesHiddenQuality is
-- the other: CR 118.8c spells "cards with a stated quality" exactly as CR
-- 701.23b does, so one predicate answers both.
--
-- Derived rather than stored on the opcode: the two are not independent -- the
-- rule reads the search's own description of what it looks for -- so a stored
-- answer to THIS question could only ever disagree with the filter it reads.
-- Search.upTo is a different question, not this one stored: the card printing
-- "up to seven cards" (Denying Wind) grants the shortfall itself, so the search
-- is optional whatever this predicate says. That flag is read beside this
-- answer, never instead of it.
--
-- A quality is stated unless the predicate is TRIVIALLY TRUE, and by the type's
-- own note `And []` is the only way to write that. Hence And joins with `any` and
-- Or with `all`: one trivially-true branch of an Or makes the whole thing
-- trivially true, while an And needs only one branch to state something. `Not`
-- and `Or []` match nothing at all, so a search through either finds nothing
-- whichever answer this gives, and True is the safe one.
--
-- Exhaustive rather than a catch-all, manaValueThresholds' posture: a new atom is
-- a stated quality, but that is a claim about the atom and should be made by
-- someone reading it rather than by a wildcard.
--
-- Polymorphic in the keyword, since no arm reads one.
statesAQuality :: Filter.Filter keyword -> Bool
statesAQuality predicate = case predicate of
  Filter.And fs -> any statesAQuality fs
  Filter.Or fs -> all statesAQuality fs
  Filter.Not _ -> True
  -- A quality like any other atom's, whatever the nested filter says: a search
  -- whose predicate is this one is looking for cards with a stated quality, so CR
  -- 701.23b applies and no descent could change that.
  Filter.ControlsMoreThanYou _ _ -> True
  -- A quality whatever the operand: "with mana value X or less" describes the
  -- card as much when X is computed, or is another object's, as when it is
  -- printed.
  Filter.Measures _ -> True
  Filter.ManaValueIsEven -> True
  Filter.HasCardType _ -> True
  Filter.HasSupertype _ -> True
  Filter.HasColor _ -> True
  Filter.IsMonocolored -> True
  Filter.SharesColorWithSource -> True
  Filter.HasSubtype _ -> True
  -- CR 701.23b's "stated quality" at its sharpest -- a named card is the most
  -- specific description a search can give -- so the searcher may decline to
  -- find one that is there, and CR 701.23d's "must find" does not apply.
  Filter.HasName _ -> True
  Filter.NameWordsAtLeast _ -> True
  -- CR 701.23b for HasName's reason: "with a name originally printed in the
  -- Arabian Nights expansion" states a quality as squarely as one name does.
  Filter.HasNameOriginallyPrintedIn _ -> True
  Filter.HasKeyword _ -> True
  Filter.HasKeywordFamily _ -> True
  Filter.ControlledBy _ -> True
  Filter.ControlledByDefendingPlayer -> True
  Filter.ControlledByBound _ -> True
  Filter.ControlledByPlayer _ -> True
  Filter.ControlledByRecipient -> True
  Filter.OwnedBy _ -> True
  Filter.OwnedByRecipient -> True
  Filter.IsSource -> True
  Filter.IsObject _ -> True
  -- CR 701.23b for IsSource's reason, and unreachable from a search besides: a
  -- card in a library targets nothing.
  Filter.TargetsSource -> True
  Filter.TargetsOnlySource -> True
  Filter.HasSingleTarget -> True
  Filter.TargetsOnlyOne _ -> True
  Filter.TargetsMatching _ -> True
  Filter.TargetsPlayer _ -> True
  Filter.IsBound _ -> True
  Filter.IsTarget -> True
  Filter.SameNameAsBound _ -> True
  Filter.SameNameAsSource -> True
  Filter.SameOwnerAsSource -> True
  Filter.SameControllerAsBound _ -> True
  Filter.SameControllerAsHostOfBound _ -> True
  Filter.SharesCreatureTypeWithBound _ -> True
  -- CR 701.23b's "stated quality" for HasName's reason, one indirection along: the
  -- description is a card name whichever way the name was arrived at, so a search
  -- whose filter is this one may decline to find what it can see.
  Filter.HasChosenName -> True
  -- CR 701.23b's "stated quality" for HasColor's reason, one indirection along:
  -- the description is a colour whichever way the colour was arrived at.
  Filter.HasChosenColor -> True
  -- The arm above one characteristic over: a subtype is a stated quality whichever
  -- way the card arrived at it.
  Filter.HasChosenSubtype -> True
  -- IsObject's posture: the candidate is named by identity.
  Filter.IsLastExiledWithSource -> True
  -- CR 701.23b's "stated quality" too, and rule 702.16k's own "regardless of
  -- that object's characteristic values" is not a counter-argument: the rule
  -- excuses the PROTECTION from reading characteristics, where this predicate
  -- asks whether a search's description is trivially true, and "a card that
  -- player owns" is not.
  Filter.OfChosenPlayer -> True
  Filter.OfRelatedPlayer _ -> True
  Filter.IsPlayer _ -> True
  Filter.IsControllerOfBound _ -> True
  Filter.CardsInGraveyardAtLeast _ -> True
  Filter.IsAttacking -> True
  Filter.IsAttackingPlayer _ -> True
  Filter.IsAttackingPlaneswalker _ -> True
  Filter.IsAttackingBattle _ -> True
  Filter.IsBlocking -> True
  Filter.IsBlocked -> True
  Filter.AttackedThisTurn -> True
  Filter.DeclaredAttackerThisCombat -> True
  Filter.DeclaredAttackedThisCombat -> True
  Filter.DeclaredBlockerThisCombat -> True
  Filter.MilledThisTurn -> True
  Filter.CantCrewVehicles -> True
  Filter.DealtDamageThisTurn -> True
  Filter.EnteredThisTurn -> True
  Filter.CrewedSourceThisTurn -> True
  Filter.ConvokedSourceThisTurn -> True
  Filter.SaddledSourceThisTurn -> True
  Filter.ControlledSinceTurnBegan -> True
  -- True whatever the nest says, for ControlsMoreThanYou's reason: "attached to
  -- something" is itself a stated quality under CR 701.23b, so even the trivial
  -- nest `And []` leaves this atom stating one and no descent could change it.
  Filter.AttachedTo _ -> True
  -- True whatever the nest says, for the atom above's reason: CR 303.4b's
  -- "enchanted" is a stated quality under CR 701.23b, and the trivial nest states
  -- one too ("has something attached to it").
  Filter.HasAttached _ -> True
  Filter.IsAttachedToSource -> True
  Filter.IsAttachedToEvaluated -> True
  Filter.IsHostOfSource -> True
  Filter.EnteredWithSource -> True
  Filter.AttachedNoLaterThanSource -> True
  Filter.CanHostSubject -> True
  Filter.CanAttachToSubject -> True
  -- True for the two atoms above's reason and not because it describes the
  -- candidate: it does not, and what CR 701.23b asks is only whether the
  -- predicate is trivially true, which no atom is. Unreachable from a search
  -- either way, the lint keeping this one at an attach's destination.
  Filter.HostOfSubjectHasCardType _ -> True
  Filter.IsCommander -> True
  Filter.IsToken -> True
  Filter.IsActivatedAbility -> True
  Filter.IsAbility -> True
  Filter.IsEmblem -> True
  -- True whatever the nest says, RepresentedByCard's reason one arm down.
  Filter.FromSource _ -> True
  Filter.IsTapped -> True
  Filter.IsFaceDown -> True
  -- True whatever the nest says, AttachedTo's reason: "is represented by a card"
  -- is itself a stated quality under CR 701.23b, and the trivial nest states it.
  Filter.RepresentedByCard _ -> True
  Filter.IsExiledFaceDown -> True
  Filter.Transformed -> True
  Filter.IsRingBearer -> True
  Filter.IsPaired -> True
  Filter.IsPairedWithSource -> True
  Filter.IsBlockedBySource -> True
  Filter.HasDesignation _ -> True
  Filter.HasCounters _ -> True
  Filter.HasCountersOfAnyKind -> True
  Filter.HasSticker _ -> True
  Filter.Stickered -> True
  Filter.HasNonManaActivatedAbility -> True
  Filter.HasActivatedAbility -> True
  -- CR 400.1 states a quality like any other atom here: a search whose predicate
  -- names a zone is looking for cards with a stated quality, so CR 701.23b's
  -- shortfall applies rather than CR 701.23d's "must find".
  Filter.IsInZone _ -> True
  -- CR 601.2a states a quality exactly as the zone atom above does, so CR
  -- 701.23b's shortfall applies rather than CR 701.23d's "must find". Unreachable
  -- from a search in practice -- a card sitting in a library was never cast -- and
  -- stated rather than left to a wildcard because there is no wildcard here.
  Filter.WasCastFrom _ -> True
  -- CR 601.2h states a quality exactly as the two atoms above do, and is
  -- unreachable from a search for their reason: a card sitting in a library was
  -- never paid for.
  Filter.TagWasSpent _ -> True
  Filter.Kicked -> True

-- Does the Filter compare a candidate against anything Context holds about
-- the SOURCE that a printed face can be told apart by -- its projected power,
-- toughness, mana value, colours or names, or a value it chose -- so that
-- matching it under two different sources may admit different candidates?
-- Pawl.Engine.Replacement.readsSource's question for a draw-replacing wish (CR
-- 616.1), whose filter Pawl.Engine.Event.eligible matches under
-- Pawl.Engine.Projection.sourceContext. The other source atoms read the
-- candidate's identity, owner or host, which a card outside the game lacks.
--
-- Every nest is descended into, since `matches` judges a nest in the same
-- Context; statesAQuality's exhaustive posture, for its reason.
readsSourceValues :: Filter.Filter keyword -> Bool
readsSourceValues predicate = case predicate of
  Filter.And fs -> any readsSourceValues fs
  Filter.Or fs -> any readsSourceValues fs
  Filter.Not f -> readsSourceValues f
  Filter.ControlsMoreThanYou _ f -> readsSourceValues f
  Filter.Measures m -> case Measures.operand m of
    Operand.OfSource _ -> True
    Operand.Literal _ -> False
    Operand.Own _ -> False
    Operand.OfBound _ -> False
    Operand.AmountInSlot _ -> False
    Operand.EnclosingAmount -> False
  Filter.ManaValueIsEven -> False
  Filter.HasCardType _ -> False
  Filter.HasSupertype _ -> False
  Filter.HasColor _ -> False
  Filter.IsMonocolored -> False
  Filter.SharesColorWithSource -> True
  Filter.HasSubtype _ -> False
  Filter.HasName _ -> False
  Filter.NameWordsAtLeast _ -> False
  Filter.HasNameOriginallyPrintedIn _ -> False
  -- A keyword's own Filter is compared, never matched (statesAQuality's
  -- HasKeyword arm), so nothing inside it reads the context.
  Filter.HasKeyword _ -> False
  Filter.HasKeywordFamily _ -> False
  Filter.ControlledBy _ -> False
  Filter.ControlledByDefendingPlayer -> False
  Filter.ControlledByBound _ -> False
  Filter.ControlledByPlayer _ -> False
  Filter.ControlledByRecipient -> False
  Filter.OwnedBy _ -> False
  Filter.OwnedByRecipient -> False
  Filter.IsSource -> False
  Filter.IsObject _ -> False
  Filter.TargetsSource -> False
  Filter.TargetsOnlySource -> False
  Filter.HasSingleTarget -> False
  Filter.TargetsOnlyOne f -> readsSourceValues f
  Filter.TargetsMatching f -> readsSourceValues f
  Filter.TargetsPlayer _ -> False
  Filter.IsBound _ -> False
  Filter.IsTarget -> False
  Filter.SameNameAsBound _ -> False
  Filter.SameNameAsSource -> True
  Filter.SameOwnerAsSource -> False
  Filter.SameControllerAsBound _ -> False
  Filter.SameControllerAsHostOfBound _ -> False
  Filter.SharesCreatureTypeWithBound _ -> False
  Filter.HasChosenName -> True
  Filter.HasChosenColor -> True
  Filter.HasChosenSubtype -> True
  Filter.IsLastExiledWithSource -> False
  Filter.OfChosenPlayer -> False
  Filter.OfRelatedPlayer _ -> False
  Filter.IsPlayer _ -> False
  Filter.IsControllerOfBound _ -> False
  Filter.CardsInGraveyardAtLeast _ -> False
  Filter.IsAttacking -> False
  Filter.IsAttackingPlayer _ -> False
  Filter.IsAttackingPlaneswalker _ -> False
  Filter.IsAttackingBattle _ -> False
  Filter.IsBlocking -> False
  Filter.IsBlocked -> False
  Filter.AttackedThisTurn -> False
  Filter.DeclaredAttackerThisCombat -> False
  Filter.DeclaredAttackedThisCombat -> False
  Filter.DeclaredBlockerThisCombat -> False
  Filter.MilledThisTurn -> False
  Filter.CantCrewVehicles -> False
  Filter.DealtDamageThisTurn -> False
  Filter.EnteredThisTurn -> False
  Filter.CrewedSourceThisTurn -> False
  Filter.ConvokedSourceThisTurn -> False
  Filter.SaddledSourceThisTurn -> False
  Filter.ControlledSinceTurnBegan -> False
  Filter.AttachedTo f -> readsSourceValues f
  Filter.HasAttached f -> readsSourceValues f
  Filter.IsAttachedToSource -> False
  Filter.IsAttachedToEvaluated -> False
  Filter.IsHostOfSource -> False
  Filter.EnteredWithSource -> False
  Filter.AttachedNoLaterThanSource -> False
  Filter.CanHostSubject -> False
  Filter.CanAttachToSubject -> False
  Filter.HostOfSubjectHasCardType _ -> False
  Filter.IsCommander -> False
  Filter.IsToken -> False
  Filter.IsActivatedAbility -> False
  Filter.IsAbility -> False
  Filter.IsEmblem -> False
  Filter.FromSource f -> readsSourceValues f
  Filter.IsTapped -> False
  Filter.IsFaceDown -> False
  Filter.RepresentedByCard f -> readsSourceValues f
  Filter.IsExiledFaceDown -> False
  Filter.Transformed -> False
  Filter.IsRingBearer -> False
  Filter.IsPaired -> False
  Filter.IsPairedWithSource -> False
  Filter.IsBlockedBySource -> False
  Filter.HasDesignation _ -> False
  Filter.HasCounters _ -> False
  Filter.HasCountersOfAnyKind -> False
  Filter.HasSticker _ -> False
  Filter.Stickered -> False
  Filter.HasNonManaActivatedAbility -> False
  Filter.HasActivatedAbility -> False
  Filter.IsInZone _ -> False
  Filter.WasCastFrom _ -> False
  Filter.TagWasSpent _ -> False
  Filter.Kicked -> False

-- The slots a Filter READS. Pawl.Engine.Resolve.Slots.modeSlots folds this over a
-- mode's target slots, which is what makes the card dataflow lint see a slot
-- named in a FILTER rather than in an effect: a card reading "that player" under
-- a condition that never binds one is then a failing test rather than a slot that
-- silently admits nothing. Pawl.Engine.Resolve.Slots.replacementRowReads is the second,
-- behavioural consumer -- see overBoundSlotsWith.
boundSlots :: Filter.Filter Keyword.Type.Keyword -> Set.Set SlotName.SlotName
boundSlots = Const.getConst . overBoundSlots (Const.Const . Set.singleton)

-- Every slot a Filter NAMES, with the arity its atom reads it at, as one
-- exhaustive traversal: `boundSlots` READS the names, `renameBound` REWRITES
-- them (CR 700.2d), `singularSlots` keeps the ones read as ONE, and `slotArities`
-- hands all of them to the dataflow and arity lints. One walk, because the
-- consumers disagreeing about which atoms name a slot is a live defect shape
-- (#2802), and with no fallthrough, so a new atom naming a slot is named by
-- -Werror rather than absorbed. That matters beyond the lints:
-- Pawl.Engine.Resolve.Slots.replacementRowReads decides which of the installing
-- resolution's bindings a waiting replacement CAPTURES through here, and
-- Pawl.Engine.Target.jointlyJudged fires CR 601.2c's joint check on
-- boundSlots, which is the whole of what enforces SameControllerAsBound and
-- SharesCreatureTypeWithBound (the offer widens for them).
--
-- The same descent `bakeBound` makes: a position that function does not bake is
-- a position this one must not report as read. NOT descended into: the Filter a
-- Keyword carries (CR 702.29e) and the one a CounterKind hides under a keyword
-- (CR 122.1b), whose evaluators supply no slots at all.
--
-- ONE: the atom asks a single thing of the slot -- CR 208.1's toughness, CR
-- 110.2's one controller or one player -- and answers nothing for a slot naming
-- several (Binding.onlyOne's doctrine; Filter.slotOneObject). MANY: the atom
-- takes the whole set the slot names (IsBound, SameNameAsBound and the rest).
-- AMOUNT: the slot holds a number (Context's boundAmounts), never a recipient.
overBoundSlotsWith :: (Applicative f) => (SlotArity.SlotArity -> SlotName.SlotName -> f SlotName.SlotName) -> Filter.Filter Keyword.Type.Keyword -> f (Filter.Filter Keyword.Type.Keyword)
overBoundSlotsWith f predicate = case predicate of
  Filter.HasCardType _ -> pure predicate
  Filter.HasSupertype _ -> pure predicate
  Filter.HasColor _ -> pure predicate
  Filter.IsMonocolored -> pure predicate
  Filter.SharesColorWithSource -> pure predicate
  Filter.HasSubtype _ -> pure predicate
  Filter.HasName _ -> pure predicate
  Filter.HasNameOriginallyPrintedIn _ -> pure predicate
  Filter.NameWordsAtLeast _ -> pure predicate
  -- The keyword's own Filter, left alone for the reason above.
  Filter.HasKeyword _ -> pure predicate
  Filter.HasKeywordFamily _ -> pure predicate
  Filter.Measures m ->
    let rebuild x = Filter.Measures m {Measures.operand = x}
     in case Measures.operand m of
          -- The slot holds an AMOUNT (Context's boundAmounts), never a recipient.
          Operand.AmountInSlot slot -> rebuild . Operand.AmountInSlot <$> f SlotArity.Amount slot
          -- CR 208.1's comparison wants ONE object's number, and
          -- Pawl.Engine.Projection.framedBySlots declines a slot that names several.
          Operand.OfBound bound -> (\slot -> rebuild (Operand.OfBound bound {BoundMeasure.slot = slot})) <$> f SlotArity.One (BoundMeasure.slot bound)
          Operand.Literal _ -> pure predicate
          Operand.OfSource _ -> pure predicate
          Operand.Own _ -> pure predicate
          Operand.EnclosingAmount -> pure predicate
  Filter.ManaValueIsEven -> pure predicate
  Filter.ControlledBy _ -> pure predicate
  Filter.ControlledByDefendingPlayer -> pure predicate
  -- A PLAYER slot, read singly all the same: `matches` answers it off a slot
  -- naming exactly one player, and bakeBound off Binding.playerSlots, which is
  -- Binding.onlyOne.
  Filter.ControlledByBound slot -> fmap Filter.ControlledByBound (f SlotArity.One slot)
  Filter.ControlledByPlayer _ -> pure predicate
  Filter.ControlledByRecipient -> pure predicate
  Filter.OwnedBy _ -> pure predicate
  Filter.OwnedByRecipient -> pure predicate
  Filter.IsSource -> pure predicate
  Filter.IsObject _ -> pure predicate
  Filter.TargetsSource -> pure predicate
  Filter.TargetsOnlySource -> pure predicate
  Filter.HasSingleTarget -> pure predicate
  -- DESCENT, for AttachedTo's reason below: CR 115.1's atom carries the one
  -- target's description, which a card author writes like any other filter.
  Filter.TargetsOnlyOne g -> fmap Filter.TargetsOnlyOne (overBoundSlotsWith f g)
  Filter.TargetsMatching g -> fmap Filter.TargetsMatching (overBoundSlotsWith f g)
  Filter.TargetsPlayer _ -> pure predicate
  -- Reads the whole bound set off Filter.Context, so a group is every one of its
  -- members rather than nothing.
  Filter.IsBound slot -> fmap Filter.IsBound (f SlotArity.Many slot)
  Filter.IsTarget -> fmap (const Filter.IsTarget) (f SlotArity.Many Binding.announcedTargets)
  -- Reads the whole set too, one field over.
  Filter.SameNameAsBound slot -> fmap Filter.SameNameAsBound (f SlotArity.Many slot)
  -- Names no slot at all: CR 702.60a's comparison is against the SOURCE, whose
  -- names arrive on Filter.Context.
  Filter.SameNameAsSource -> pure predicate
  Filter.SameOwnerAsSource -> pure predicate
  -- Reads the whole set too, one field further over.
  Filter.SameControllerAsBound slot -> fmap Filter.SameControllerAsBound (f SlotArity.Many slot)
  -- Reads the whole set too, off its own field: a slot naming a group answers
  -- with every member's host's controller.
  Filter.SameControllerAsHostOfBound slot -> fmap Filter.SameControllerAsHostOfBound (f SlotArity.Many slot)
  -- Reads the whole set too, off its own field.
  Filter.SharesCreatureTypeWithBound slot -> fmap Filter.SharesCreatureTypeWithBound (f SlotArity.Many slot)
  Filter.HasChosenName -> pure predicate
  -- Reads no slot either: CR 105.2's colour arrives on Filter.Context.
  Filter.HasChosenColor -> pure predicate
  -- Reads no slot either: CR 205.3's subtype arrives on Filter.Context.
  Filter.HasChosenSubtype -> pure predicate
  Filter.IsLastExiledWithSource -> pure predicate
  -- Reads no slot at all: rule 702.16k's player arrives on Filter.Context.
  Filter.OfChosenPlayer -> pure predicate
  Filter.OfRelatedPlayer _ -> pure predicate
  Filter.IsPlayer _ -> pure predicate
  -- The candidate is the controller of the ONE object the slot names (CR
  -- 608.2h), read through slotOneObject.
  Filter.IsControllerOfBound slot -> fmap Filter.IsControllerOfBound (f SlotArity.One slot)
  -- DESCENT: the nest is card text like any other, and an atom written into it
  -- is read exactly as one written at the top level.
  Filter.ControlsMoreThanYou n g -> fmap (Filter.ControlsMoreThanYou n) (overBoundSlotsWith f g)
  Filter.CardsInGraveyardAtLeast _ -> pure predicate
  Filter.IsAttacking -> pure predicate
  Filter.IsAttackingPlayer _ -> pure predicate
  Filter.IsAttackingPlaneswalker _ -> pure predicate
  Filter.IsAttackingBattle _ -> pure predicate
  Filter.DeclaredAttackedThisCombat -> pure predicate
  Filter.IsBlocking -> pure predicate
  Filter.IsBlocked -> pure predicate
  Filter.DeclaredAttackerThisCombat -> pure predicate
  Filter.DeclaredBlockerThisCombat -> pure predicate
  Filter.AttackedThisTurn -> pure predicate
  Filter.MilledThisTurn -> pure predicate
  Filter.CantCrewVehicles -> pure predicate
  Filter.DealtDamageThisTurn -> pure predicate
  Filter.EnteredThisTurn -> pure predicate
  Filter.CrewedSourceThisTurn -> pure predicate
  Filter.ConvokedSourceThisTurn -> pure predicate
  Filter.SaddledSourceThisTurn -> pure predicate
  Filter.ControlledSinceTurnBegan -> pure predicate
  -- DESCENT, for ControlsMoreThanYou's reason.
  Filter.AttachedTo g -> fmap Filter.AttachedTo (overBoundSlotsWith f g)
  -- DESCENT, for the atom above's reason.
  Filter.HasAttached g -> fmap Filter.HasAttached (overBoundSlotsWith f g)
  Filter.IsAttachedToSource -> pure predicate
  Filter.IsAttachedToEvaluated -> pure predicate
  Filter.IsHostOfSource -> pure predicate
  Filter.EnteredWithSource -> pure predicate
  Filter.AttachedNoLaterThanSource -> pure predicate
  Filter.CanHostSubject -> pure predicate
  Filter.CanAttachToSubject -> pure predicate
  Filter.HostOfSubjectHasCardType _ -> pure predicate
  Filter.IsCommander -> pure predicate
  Filter.IsToken -> pure predicate
  Filter.IsActivatedAbility -> pure predicate
  Filter.IsAbility -> pure predicate
  Filter.IsEmblem -> pure predicate
  -- DESCENT, for RepresentedByCard's reason below.
  Filter.FromSource g -> fmap Filter.FromSource (overBoundSlotsWith f g)
  Filter.IsTapped -> pure predicate
  Filter.IsFaceDown -> pure predicate
  -- DESCENT, for the atom above's reason.
  Filter.RepresentedByCard g -> fmap Filter.RepresentedByCard (overBoundSlotsWith f g)
  Filter.IsExiledFaceDown -> pure predicate
  Filter.Transformed -> pure predicate
  Filter.IsRingBearer -> pure predicate
  Filter.IsPaired -> pure predicate
  Filter.IsPairedWithSource -> pure predicate
  Filter.IsBlockedBySource -> pure predicate
  Filter.HasDesignation _ -> pure predicate
  -- The kind may be a whole Keyword hiding a Filter, left alone for the reason
  -- the keyword atom above is.
  Filter.HasCounters _ -> pure predicate
  Filter.HasCountersOfAnyKind -> pure predicate
  Filter.HasSticker _ -> pure predicate
  Filter.Stickered -> pure predicate
  Filter.HasNonManaActivatedAbility -> pure predicate
  Filter.HasActivatedAbility -> pure predicate
  Filter.IsInZone _ -> pure predicate
  Filter.WasCastFrom _ -> pure predicate
  Filter.TagWasSpent _ -> pure predicate
  Filter.Kicked -> pure predicate
  Filter.And fs -> fmap Filter.And (traverse (overBoundSlotsWith f) fs)
  Filter.Or fs -> fmap Filter.Or (traverse (overBoundSlotsWith f) fs)
  Filter.Not g -> fmap Filter.Not (overBoundSlotsWith f g)

-- overBoundSlotsWith without the arity: CR 700.2d's rename.
overBoundSlots :: (Applicative f) => (SlotName.SlotName -> f SlotName.SlotName) -> Filter.Filter Keyword.Type.Keyword -> f (Filter.Filter Keyword.Type.Keyword)
overBoundSlots f = overBoundSlotsWith (const f)

-- Every slot a Filter names, each at the narrowest arity it is read at.
slotArities :: Filter.Filter Keyword.Type.Keyword -> Map.Map SlotName.SlotName SlotArity.SlotArity
slotArities = Map.fromListWith min . Const.getConst . overBoundSlotsWith (\arity slot -> Const.Const [(slot, arity)])

-- The slots a Filter reads as ONE object or ONE player -- the classification
-- Pawl.Engine.Resolve.Slots.filterSlotsOf's arity lint and Pawl.CardSpec's
-- clash lint both read.
singularSlots :: Filter.Filter Keyword.Type.Keyword -> Set.Set SlotName.SlotName
singularSlots = Map.keysSet . Map.filter (== SlotArity.One) . slotArities

-- Every slot NAME a Filter carries, rewritten. CR 700.2d's per-occurrence rename
-- is the caller (Pawl.Engine.Modal.instanceScope): a mode chosen twice renames
-- its slots per occurrence, so a filter naming a SIBLING slot -- Fall of the
-- Hammer's "another target creature" is Not (IsBound "dealer") -- would otherwise
-- name occurrence 0's slot from occurrence 1 and read a binding that is not its
-- own. The function it is given is a PARTIAL rename (Modal.ownSlot): a name the
-- mode does not declare belongs to CR 603.2's environment, which no occurrence
-- copies, and must come back unchanged.
renameBound :: (SlotName.SlotName -> SlotName.SlotName) -> Filter.Filter Keyword.Type.Keyword -> Filter.Filter Keyword.Type.Keyword
renameBound rename = Identity.runIdentity . overBoundSlots (Identity.Identity . rename)

-- CR 702.16k's three disjuncts for the players `named` picks: the candidate's
-- controller, its owner while no player controls it (CR 108.4), and the
-- controller of the spell or ability aiming it, where one is.
ofPlayer :: (Maybe PlayerId.PlayerId -> Bool) -> Maybe (Maybe PlayerId.PlayerId) -> View -> Bool
ofPlayer named aimer view =
  named (controller view)
    || (named (owner view) && Maybe.isNothing (controller view))
    || maybe False named aimer
