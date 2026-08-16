module Pawl.Engine.Filter where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Cycling as Cycling
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.DiscardCards as DiscardCards
import qualified Pawl.Types.ExileCardsFromGraveyard as ExileCardsFromGraveyard
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.Morph as Morph
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Reinforce as Reinforce
import qualified Pawl.Types.Sacrifice as Sacrifice
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype
import qualified Pawl.Types.TapForTotalPower as TapForTotalPower

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
    -- CR 202.3: the candidate's mana value (CR 202.3a gives a costless object
    -- 0). On the battlefield it comes off the CR 613 projection, so CR 707.2's
    -- copiable mana cost is honoured -- a Clone reports what it copied. Off the
    -- battlefield it is the printed cost's, and unlike `power` it is NOT Nothing
    -- there -- a mana cost is printed on the card and rule 202.3 names no zone
    -- -- which is what lets ManaValueAtMost filter a graveyard.
    --
    -- Nothing where there is no card to read: a player view, or an object with
    -- no card behind it such as an ability on the stack.
    manaValue :: Maybe Integer,
    controller :: Maybe PlayerId.PlayerId,
    -- CR 108.3 / 110.2: the candidate's OWNER -- the player who started the game
    -- with the card in their deck, or (CR 111.2) the player who created the
    -- token. Read straight off Object.owner, which setup writes once and no rule
    -- ever rewrites: CR 613.1b's layer 2 changes CONTROL, and rule 108.3 has no
    -- counterpart, so no projection is consulted.
    --
    -- Just in strictly MORE places than `controller` above, and deliberately: an
    -- owner is a fact about a card IN THE GAME, so it is answerable in every
    -- zone, where CR 108.4 gives a card outside the battlefield and the stack no
    -- controller at all. That is manaValue's posture rather than power's, for
    -- manaValue's reason.
    --
    -- Nothing only where there is no OBJECT to read it off: a player view, an
    -- event snapshot, or a printed card being matched by a search, which CR
    -- 109.1 makes an object of nothing. OwnedBy is vacuously False there, the
    -- posture power and controller take.
    owner :: Maybe PlayerId.PlayerId,
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
    -- GameState.combat rather than from a projection. False for every candidate
    -- with no combat status to read: a printed card off the battlefield, a
    -- player, an event snapshot -- the vacuous posture power and controller take.
    attacking :: Bool,
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
    -- CR 608.2i: was this candidate declared as an attacker earlier this turn?
    -- Unlike `attacking` not even a present state: it is a look-back read of the
    -- turn-scoped GameEvent log.
    --
    -- LAZY, like `attachedToCreature` below but for a plainer reason: filling it
    -- folds the whole turn's event log, and nothing forces it unless a Filter
    -- actually contains AttackedThisTurn. That is a cost argument rather than
    -- the recursion hazard the next field records.
    attackedThisTurn :: Bool,
    -- CR 701.17a: was this candidate MILLED earlier this turn? A look-back read
    -- of the same log `attackedThisTurn` above reads, and LAZY for that field's
    -- reason -- nothing forces it unless a Filter contains MilledThisTurn.
    milledThisTurn :: Bool,
    -- CR 701.3a: is this candidate attached to a CREATURE right now? Not a
    -- characteristic either (CR 109.3), so it is read from Object.attachedTo plus
    -- the HOST's projected card types rather than from the candidate's own
    -- projection.
    --
    -- LAZY, and load-bearingly so. Filling it costs a projection OF ANOTHER
    -- OBJECT, and viewOfCharacteristics is itself called from inside
    -- Projection.affects while a projection is being computed. Nothing forces
    -- this field unless a Filter actually contains IsAttachedToCreature, and no
    -- affected-set filter in the pool does; one that did would recurse back into
    -- the projection that is asking. A fact about the pool's card data rather than
    -- a guarantee this record enforces (#357).
    attachedToCreature :: Bool,
    -- CR 303.4: is this candidate attached to a PERMANENT right now? Read from
    -- Object.attachedTo alone -- whether the attachment names an object rather
    -- than a player, which is what Recipient.objectOf asks. Unlike
    -- `attachedToCreature` this reads no second projection, so it needs no
    -- laziness argument.
    attachedToPermanent :: Bool,
    -- CR 701.3a / 301.5a: WHICH object this candidate is attached to, for
    -- IsAttachedToSource to compare against Context.source -- the id and not a
    -- Bool, because the atom's answer depends on the match's source and this
    -- record is built once per candidate.
    --
    -- Nothing where Object.attachedTo is, and also where it names a PLAYER (CR
    -- 303.4's other destination), which is Recipient.objectOf's Nothing. Reads no
    -- second projection, so unlike `attachedToCreature` it needs no laziness
    -- argument -- an ObjectId is not a characteristic.
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
    -- reading of the source, the same for every candidate in the match. Pawl.Engine.Attach.hostsFor is the only
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
    -- Widening the subject to somewhere every evaluation can see it is #572.
    canHostSubject :: Bool,
    -- CR 111.1 / 111.6: is this candidate a token rather than a card? Read from
    -- Object.source (Pawl.Engine.Game.isToken), never from a projection -- CR 111.3 makes
    -- a token's effect-defined characteristics equivalent to printed ones, so no
    -- characteristic axis distinguishes the two and no CR 613 layer can change the
    -- answer. False for every candidate with no object behind it.
    token :: Bool,
    -- | CR 110.5a's tap status. Not a characteristic, so no projection writes it;
    -- read straight off the object.
    tapped :: Bool,
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
    -- battlefield, a player, an event snapshot.
    counters :: Map.Map (CounterKind.CounterKind Keyword.Type.Keyword) Natural.Natural,
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
    -- monstrosity's clause condition, Repeat Offender's). What CR 701.60c hangs off
    -- `Suspected` does NOT come through here:
    -- Pawl.Engine.Projection.designationGathered and
    -- Pawl.Engine.CombatRestriction.inForce hold no view and read the object
    -- directly.
    designations :: Set.Set Designation.Designation,
    -- CR 702.33d: has this candidate been kicked? Read off Object.kicked, and
    -- False where there is no object to read it off, both for the reasons
    -- `designations` above gives. Its one reader is Pawl.Engine.Quantity's WasKicked
    -- arm, answering Burst Lightning's clause conditions and Monstrous War-Leech's
    -- CR 604.2 clause on its entry replacement.
    --
    -- Not a designation of a PERMANENT as that field holds -- rule 702.33d
    -- designates the SPELL -- but it comes through the view for the same reason
    -- those do: the reader holds a view and not a board. It is nonetheless True
    -- for a permanent a kicked spell became, which is CR 400.7d's exception to
    -- the forgetting (see Pawl.Types.Object).
    kicked :: Bool,
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
    -- LAZY, for Pawl.Engine.Projection.viewOfCharacteristics' attachedToCreature
    -- reason: filling it re-asks CR 702.178a's grant condition, which reaches a
    -- second projection, and `affects` builds a view from inside a projection
    -- already. Nothing forces it unless a Filter actually contains the atom, and
    -- the pool's one printing (Tsabo's Web) is read outside the layer fold; an
    -- affected-set filter that used it would recurse.
    nonManaActivatedAbility :: Bool
  }
  deriving (Eq, Show)

-- The view of a PLAYER candidate: no card types, no colours, no controller --
-- a player is not an object (CR 109.1) and has none of those. Only the player's
-- own identity is answerable, which is exactly what IsPlayer asks.
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
      -- CR 202.3 reads a mana cost, which is printed on an OBJECT (CR 202.1); a
      -- player has none.
      manaValue = Nothing,
      controller = Nothing,
      -- CR 108.3 gives an owner to a CARD; a player owns cards and is not one.
      owner = Nothing,
      identity = Nothing,
      playerIdentity = Just pid,
      -- CR 506.3: only a creature can attack, and a player is not one.
      attacking = False,
      -- CR 509.1a: only a creature can block, either.
      blocking = False,
      -- CR 509.1h: blocked-ness is a status of an ATTACKING creature, and by CR
      -- 506.3 a player never is one.
      blocked = False,
      -- CR 506.3 again: a player was never declared as an attacker either.
      attackedThisTurn = False,
      -- CR 701.17a mills CARDS, and a player is not one.
      milledThisTurn = False,
      -- CR 303.4b: a player an Aura is attached to is ENCHANTED by it; the
      -- player is not itself attached to anything, because Object.attachedTo is
      -- a field of the ATTACHED permanent, and a player is not one.
      attachedToCreature = False,
      -- CR 303.4 again, for the same reason.
      attachedToPermanent = False,
      -- CR 303.4 a third time: a player is attached to nothing, so there is no
      -- host id for IsAttachedToSource to compare.
      attachedTo = Nothing,
      -- CR 701.3a's question can be asked about a player (CR 702.5d), but not
      -- here: the only site that fills this field is Pawl.Engine.Resolve's
      -- AttachTarget arm, whose candidates are battlefield permanents.
      canHostSubject = False,
      -- CR 111.1: a token represents a PERMANENT, and a player is not one.
      token = False,
      tapped = False,
      -- CR 122.1 puts counters on an object OR a player, and a player's are
      -- Player.counters, read by Quantity.PlayerCounters. This field is the
      -- OBJECT half, so a player view has none of it.
      counters = Map.empty,
      -- CR 701.54b: Ring-bearer is a designation A PERMANENT can have, and a
      -- player is not one -- the same shape CR 725.1's monarch has with the two
      -- sides swapped.
      ringBearerFor = Nothing,
      -- CR 702.112b: "only permanents can be or become renowned", CR 701.37b and
      -- CR 701.60b saying the same of the other two, and a player is not one.
      designations = Set.empty,
      kicked = False,
      -- CR 602.1: an activated ability is an ability OF AN OBJECT, and CR 109.1's
      -- list of what an object is has no player in it -- `keywords` above, one
      -- rule over.
      nonManaActivatedAbility = False
    }

-- The perspective the match is relative to: who counts as "you" (CR 109.5), and
-- which object the surrounding effect comes from. Both are Nothing when no
-- player and no source frame the match (an off-battlefield search).
data Context = MkContext
  { perspective :: Maybe PlayerId.PlayerId,
    source :: Maybe ObjectId.ObjectId,
    -- CR 208.1: the SOURCE's power, for the two atoms that compare a candidate
    -- against it (PowerLessThanSource, CR 702.134a; PowerGreaterThanSource, CR
    -- 702.149a). Not derivable from `source` here -- this module holds no game
    -- state and cannot project -- so the caller that has the board supplies it:
    -- Pawl.Engine.Target.admittedGiven for a target slot,
    -- Pawl.Engine.Event.matchesTrigger for CR 702.149a's trigger condition, and
    -- Pawl.Engine.CombatRestriction.cantBeBlockedBy for CR 701.54c's blocking
    -- restriction.
    --
    -- LAZY, and load-bearingly so: filling it costs a projection of the source,
    -- and no filter that omits the atom ever forces it. That is the posture
    -- View.attackedThisTurn takes for its event-log fold.
    --
    -- Nothing wherever the atom cannot appear, which `contextFor` below is the
    -- spelling of.
    sourcePower :: Maybe Integer,
    -- CR 508.5: the DEFENDING PLAYER for the source, for the one atom that asks
    -- (ControlledByDefendingPlayer, CR 702.39a). Supplied by the caller for
    -- sourcePower's reason -- this module holds no game state and cannot read the
    -- combat record -- and by the same caller, Pawl.Engine.Target.admittedGiven.
    --
    -- LAZY like sourcePower, and load-bearingly so: filling it costs a
    -- control-grant walk, and no filter that omits the atom ever forces it.
    defendingPlayer :: Maybe PlayerId.PlayerId,
    -- The player the surrounding effect is CURRENTLY BEING APPLIED TO, for the one
    -- atom that asks (ControlledByRecipient) -- Biorhythm's "the number of
    -- creatures they control". Supplied by the caller for defendingPlayer's
    -- reason, and by one caller: Pawl.Engine.Resolve's SetLifeTotal arm, which
    -- re-evaluates its quantity once per recipient with this field pointed at each
    -- in turn.
    --
    -- NOT `perspective` re-pointed, which would be the cheap version of the same
    -- thing and a wrong one: CR 109.5's "you" is the resolving spell's controller
    -- for the whole quantity, so a card reading both "they control" and "you
    -- control" in one sentence needs the two to disagree.
    --
    -- Nothing wherever the atom cannot appear, which is everywhere else. Not
    -- implemented: any other per-player opcode filling it, so no card may write a
    -- per-recipient amount for a life loss, a gain, a draw or a speed (#1427).
    recipient :: Maybe PlayerId.PlayerId,
    -- The objects the surrounding resolution's LEGAL slots name, for
    -- Quantity.AgainstSlot to aim an evaluation at one (CR 608.2b keeps an
    -- illegal slot out), and for the IsBound atom above. It rides here because
    -- this record is already the evaluation context every Quantity is handed,
    -- and a slot map is exactly the part of a resolution the evaluator cannot
    -- derive.
    --
    -- EMPTY everywhere but a resolution, which is the honest answer rather than a
    -- forgotten filler: outside one there are no slots. Pawl.Engine.Resolve's
    -- effectContext is the sole non-empty producer.
    slotObjects :: Map.Map SlotName.SlotName ObjectId.ObjectId,
    -- CR 201.1 / 709.4a: the NAMES of the objects the surrounding announcement's
    -- slots hold, for the one atom that compares a candidate's against them
    -- (SameNameAsBound, Harness the Storm). Supplied by the caller for
    -- sourcePower's reason -- this module holds no game state and cannot read an
    -- object's names -- and by one caller, Pawl.Engine.Target.admittedGiven,
    -- which is where a target slot's Filter is matched.
    --
    -- Separate from `slotObjects` above rather than derived from it, and that is
    -- the same division sourcePower makes against `source`: an id is not a name
    -- until a board has been asked.
    --
    -- LAZY, and load-bearingly so: filling it costs one projection per bound
    -- object, and no filter that omits the atom ever forces it.
    --
    -- EMPTY in contextFor and contextWithSlots below, so the atom is vacuously
    -- False in every position but a target slot -- the posture every
    -- context-relative atom here takes.
    --
    -- What keeps a card out of those positions is Pawl.CardSpec's
    -- "CR 709.4a no card asks SameNameAsBound outside a mode's target slot",
    -- the sweep sourcePower's and defendingPlayer's siblings each have.
    slotNames :: Map.Map SlotName.SlotName (Set.Set CardName.CardName)
  }
  deriving (Eq, Show)

-- A Context for every match whose Filter cannot name a context-relative atom --
-- that is, every match but a target slot's, CR 702.149a's trigger condition and
-- CR 509.1b's blocking gate. The source-power atoms reach a card only through
-- Pawl.Engine.Keyword's own mentor and training and through
-- Pawl.Engine.Ring's emblem, and CR 702.39a's defending-player atom only through
-- provoke; Pawl.CardSpec's lints keep all three out of card data, so no other
-- position can read the Nothings this leaves.
--
-- CR 119.5's recipient atom is the one a CARD may write (Biorhythm), so the
-- Nothing it leaves here is reachable: a card naming "they control" outside a
-- per-recipient effect matches nothing, which is PlayerRef.Candidate's posture one
-- type over rather than a hole -- an evaluation that has reached no recipient has
-- no honest player to substitute.
contextFor :: Maybe PlayerId.PlayerId -> Maybe ObjectId.ObjectId -> Context
contextFor p s = MkContext {perspective = p, source = s, sourcePower = Nothing, defendingPlayer = Nothing, recipient = Nothing, slotObjects = Map.empty, slotNames = Map.empty}

-- contextFor with the resolution's slot objects supplied. The one caller is
-- Pawl.Engine.Resolve.effectContext; see slotObjects above.
contextWithSlots :: Maybe PlayerId.PlayerId -> Maybe ObjectId.ObjectId -> Map.Map SlotName.SlotName ObjectId.ObjectId -> Context
contextWithSlots p s m = (contextFor p s) {slotObjects = m}

-- contextFor with the source's power supplied. Kept lazy at the call site, since
-- the field is: a Filter that never names the atom pays for no projection.
--
-- The defending player stays Nothing on both callers -- CR 702.149a's TRIGGER
-- match and CR 509.1b's blocking gate -- since rule 702.39a's atom lives only in a
-- target slot.
contextComparingPower :: Maybe PlayerId.PlayerId -> ObjectId.ObjectId -> Maybe Integer -> Context
contextComparingPower p s n = MkContext {perspective = p, source = Just s, sourcePower = n, defendingPlayer = Nothing, recipient = Nothing, slotObjects = Map.empty, slotNames = Map.empty}

-- The one generic matcher. A pure fold over the Filter tree; it never inspects
-- which effect produced the Filter. Identity checks like IsSource consult the
-- supplied Context, not information baked into the predicate.
matches :: Context -> View -> Filter.Filter Keyword.Type.Keyword -> Bool
matches context view predicate = case predicate of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.HasSubtype s -> Set.member s (subtypes view)
  -- CR 709.4a's own test, said the way that rule says it: membership, so an
  -- object showing several names matches on any one of them.
  Filter.HasName n -> Set.member n (names view)
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
  Filter.PowerAtLeast n -> case power view of
    Nothing -> False
    Just p -> p >= n
  -- CR 208.1 again, and False for the same absent power PowerAtLeast declines:
  -- an object with no power is not "a creature with power 2 or less", it is not
  -- a creature at all.
  Filter.PowerAtMost n -> case power view of
    Nothing -> False
    Just p -> p <= n
  -- CR 702.134a's "power less than this creature's power", the one power
  -- comparison whose bound is another object rather than a literal. False unless
  -- BOTH powers are readable, which is the two arms above joined: a candidate
  -- with no power is no more "a creature with lesser power" than it is one with
  -- power 2 or less, and a source whose power nothing supplied names no bound to
  -- be less than.
  Filter.PowerLessThanSource -> case (power view, sourcePower context) of
    (Just p, Just s) -> p < s
    _ -> False
  -- CR 702.149a's "power greater than this creature's power", the same comparison
  -- reversed, and False on an absent power at either end for the same reason.
  Filter.PowerGreaterThanSource -> case (power view, sourcePower context) of
    (Just p, Just s) -> p > s
    _ -> False
  -- CR 202.3, and answerable in every zone -- see the View field's own note.
  -- Vacuously False for a player, which has no mana value to compare.
  Filter.ManaValueAtMost n -> case manaValue view of
    Nothing -> False
    Just mv -> mv <= n
  -- CR 202.3 again, read for parity. Void Winnower's reminder text is the
  -- boundary case in the rulebook's own words -- "(Zero is even.)" -- and `even
  -- 0` agrees, which is also CR 202.3a's answer for an object with no mana cost:
  -- an animated land has a mana value of 0 and so an EVEN one.
  --
  -- Vacuously False where there is no mana value at all, exactly as the atom
  -- above is: a player, or an object with no card behind it.
  Filter.ManaValueIsEven -> maybe False even (manaValue view)
  -- Every other player is an Opponent by construction: CR 806.1 has a
  -- free-for-all's players compete as individuals against each other, and CR
  -- 102.2 says the same for two players -- one predicate, `c /= p`, serves both.
  -- CR 102.3's teams are the ONE reading it is wrong for, and pawl has none to
  -- express (#175). Unlike Pawl.Engine.Count.playersFor, which folds a player
  -- SET, this arm tests one candidate `View` at a time, so there is no set here
  -- to get the size of wrong.
  Filter.ControlledBy relation -> case (controller view, perspective context) of
    (Just c, Just p) -> case relation of
      PlayerRelation.You -> c == p
      PlayerRelation.Opponent -> c /= p
    _ -> False
  -- CR 508.5 / 702.39a: the candidate's controller IS the defending player, which
  -- the Context supplies because it is a fact about the combat record rather than
  -- about the candidate. False unless both are readable, the posture
  -- PowerLessThanSource takes: a candidate with no controller and a source with no
  -- defending player each leave nothing to compare.
  Filter.ControlledByDefendingPlayer -> case (controller view, defendingPlayer context) of
    (Just c, Just d) -> c == d
    _ -> False
  -- CR 603.2's "that player controls", and False WHEREVER IT IS REACHED: `bakeBound`
  -- below replaces the atom with the arm under it before either of CR 115's moments
  -- judges the slot, so an atom that survives to here is one whose slot named no one
  -- player. That is the vacuous posture every player-referencing atom takes, and it
  -- is why this reads no Context field -- the substitution happens where the bindings
  -- are, which is not here.
  Filter.ControlledByBound _ -> False
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
  -- conjoined. Every other player is an Opponent by construction, for the reason
  -- the arm above gives, and CR 102.3's teams are the one reading it is wrong for
  -- (#175). Vacuously False where no object backs the view, or where no
  -- perspective frames the match.
  Filter.OwnedBy relation -> case (owner view, perspective context) of
    (Just o, Just p) -> case relation of
      PlayerRelation.You -> o == p
      PlayerRelation.Opponent -> o /= p
    _ -> False
  Filter.IsSource -> case (identity view, source context) of
    (Just oid, Just src) -> oid == src
    _ -> False
  -- IsSource one field over: the id the RESOLUTION bound rather than the id the
  -- evaluation is sourced at. Vacuously False for a view with no object behind
  -- it and for a slot naming nothing, which is the posture the atom above takes.
  Filter.IsBound slot -> case (identity view, Map.lookup slot (slotObjects context)) of
    (Just oid, Just bound) -> oid == bound
    _ -> False
  -- CR 709.4a at both ends: the candidate has the bound object's name if one of
  -- its names is one of that object's, which is a non-empty INTERSECTION. A slot
  -- naming nothing, and a bound object with no name (CR 708.2a), each leave the
  -- other side empty and answer False without a case of their own.
  Filter.SameNameAsBound slot -> not (Set.disjoint (names view) (Map.findWithDefault Set.empty slot (slotNames context)))
  -- CR 115.1's "target opponent". Same "every other player is an opponent"
  -- reading the ControlledBy arm above argues for, and wrong for the same one
  -- case (CR 102.3's teams, #175). Vacuously False for an object candidate,
  -- which has no playerIdentity, and for a match with no perspective.
  Filter.IsPlayer relation -> case (playerIdentity view, perspective context) of
    (Just candidate, Just you) -> case relation of
      PlayerRelation.You -> candidate == you
      PlayerRelation.Opponent -> candidate /= you
    _ -> False
  -- The controller of the object a slot names, and False WHEREVER IT IS REACHED,
  -- for ControlsMoreThanYou's reason below: Pawl.Engine.Count.bakePerspective
  -- answers it against the board, and this module holds none. An atom that
  -- survives to here is one in a position nothing bakes -- any filter but a
  -- Scope.OverPlayers count's.
  Filter.IsControllerOfBound _ -> False
  -- CR 110.2's board comparison, and False WHEREVER IT IS REACHED, exactly as
  -- ControlledByBound above is: Pawl.Engine.Count.bakePerspective replaces the
  -- atom with a trivially true or trivially false predicate before the candidate
  -- is matched, because answering it means counting permanents and this module
  -- holds no game state. An atom that survives to here is one in a position
  -- nothing bakes -- any filter but a Scope.OverPlayers count's.
  Filter.ControlsMoreThanYou _ -> False
  -- CR 508.1k: a creature stays attacking until it is removed from combat or the
  -- combat phase ends, so this is a live read of the combat record, never a stamp
  -- on the object.
  Filter.IsAttacking -> attacking view
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
  -- CR 701.17a: the same look-back, over the mills rather than the attacks. Like
  -- the atom above it cannot stop being true within a turn, and unlike it the
  -- candidate can stop EXISTING -- CR 400.7 mints a new object the moment the
  -- milled card moves again, and the new one was not milled.
  Filter.MilledThisTurn -> milledThisTurn view
  -- CR 701.3a: a live read of Object.attachedTo and the host's projected types,
  -- never a stamp on the candidate -- an Aura whose host stops being a creature
  -- stops matching, and CR 704.5m buries it on the next state-based-action pass.
  Filter.IsAttachedToCreature -> attachedToCreature view
  -- CR 303.4: a live read of Object.attachedTo, and of nothing else -- whether the
  -- attachment names an object rather than a player. An Aura buried by CR 704.5m
  -- stops matching because it stops being attached, never because a stamp was
  -- cleared.
  Filter.IsAttachedToPermanent -> attachedToPermanent view
  -- CR 701.3a / 301.5a: IsSource's comparison in the other direction -- the
  -- candidate's HOST against the match's source, rather than the candidate itself.
  -- A live read of Object.attachedTo, so an Equipment unequipped by CR 704.5n
  -- stops matching at once. Vacuously False where the candidate is attached to
  -- nothing or to a player, and where no source frames the match.
  Filter.IsAttachedToSource -> case (attachedTo view, source context) of
    (Just host, Just src) -> host == src
    _ -> False
  -- CR 701.3a: a live read of the legality of the attach this match is framing,
  -- computed by the caller that knows what is moving. Vacuously False outside one.
  Filter.CanHostSubject -> canHostSubject view
  -- CR 111.6: a token isn't a card. A live read of what the object is
  -- represented by (Object.source), never a stamp on the candidate -- and unlike
  -- the two arms above it cannot change while the game runs, because CR 111.3
  -- makes a token's characteristics equivalent to a card's.
  Filter.IsToken -> token view
  Filter.IsTapped -> tapped view
  -- CR 602.1 with CR 605.1a's exclusion, both applied by the builder that holds
  -- the abilities. A live read: the projection is re-asked on every match, so a
  -- land Humility has stripped stops matching at once, and CR 702.29b's and CR
  -- 702.77b's abilities are in the list the builder measured.
  Filter.HasNonManaActivatedAbility -> nonManaActivatedAbility view
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
  -- The designation, asked of the CANDIDATE. A live read of Object.designations,
  -- never a stamp on the candidate: each rule ends its designation when the
  -- permanent leaves the battlefield, and CR 400.7's new incarnation simply arrives
  -- without it. Asks nothing of the perspective, unlike the arm above -- none of
  -- these designations belongs to a player. NOT the menace or the can't-block CR
  -- 701.60c hangs off `Suspected` -- a permanent can have either from somewhere
  -- else.
  Filter.HasDesignation d -> Set.member d (designations view)
  -- CR 122.1, asked of the CANDIDATE: has it one or more counters of the kind?
  -- HasDesignation's live read, of counters instead of a designation: CR 400.7's new
  -- incarnation arrives with none, so nothing is stamped on the candidate.
  Filter.HasCounters kind -> Map.findWithDefault 0 kind (counters view) > 0
  Filter.And fs -> all (matches context view) fs
  Filter.Or fs -> any (matches context view) fs
  Filter.Not f -> not (matches context view f)

-- CR 612.1: swap subtype words wherever they appear in a Filter. A text-changing
-- effect reaches any word printed on the object, and a Filter carried by an
-- effect is part of that text -- so this is the shape
-- Pawl.Engine.Projection.rewriteModification already has, for the type THIS
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
  -- Untouched, and CR 612.2 says so outright: "an effect that changes a color
  -- word or a subtype can't change a card name, even if that name contains a
  -- word ... that is the same as a Magic color word, basic land type, or
  -- creature type". This function's pairs are exactly such a subtype swap.
  Filter.HasName _ -> predicate
  -- CR 702.14a: a keyword can hold a land-type word too, so "creature with
  -- swampwalk" is text a swap reaches exactly as "creature that's a Swamp" is.
  -- rewriteKeyword below is the descent, shared with the two sites that rewrite
  -- a keyword rather than a filter over one.
  Filter.HasKeyword k -> Filter.HasKeyword (rewriteKeyword pairs k)
  -- DESCENT, like And/Or/Not above: the nested filter describes the permanents
  -- being counted ("more LANDS than you"), so a word swap reaches it exactly as it
  -- reaches the same description written at the top level.
  Filter.ControlsMoreThanYou f -> Filter.ControlsMoreThanYou (rewrite pairs f)
  -- Untouched, where the atom above is rewritten, and the contrast is CR 612's
  -- rather than an omission: rule 612.1's swap acts on a WORD in the text, and a
  -- family names no word. Magical Hack turning "Swamp" into "Island" turns a
  -- swampwalk into an islandwalk, so "creature with swampwalk" has to follow it;
  -- "creature with landwalk" still reads landwalk afterwards, and CR 702.14a's
  -- generic term is not itself a land type to swap.
  Filter.HasKeywordFamily _ -> predicate
  Filter.PowerAtLeast _ -> predicate
  Filter.PowerAtMost _ -> predicate
  -- Untouched for the two above's reason: the atom names a comparison, and CR
  -- 612.1 finds no word in it to swap.
  Filter.PowerLessThanSource -> predicate
  Filter.PowerGreaterThanSource -> predicate
  Filter.ManaValueAtMost _ -> predicate
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
  Filter.IsSource -> predicate
  Filter.IsBound _ -> predicate
  Filter.SameNameAsBound _ -> predicate
  Filter.IsPlayer _ -> predicate
  -- Untouched for IsPlayer's reason: CR 612.1 swaps a WORD in the text, and this
  -- atom names a slot rather than a subtype.
  Filter.IsControllerOfBound _ -> predicate
  Filter.IsAttacking -> predicate
  Filter.IsBlocking -> predicate
  Filter.IsBlocked -> predicate
  Filter.AttackedThisTurn -> predicate
  Filter.MilledThisTurn -> predicate
  Filter.IsAttachedToCreature -> predicate
  Filter.IsAttachedToPermanent -> predicate
  Filter.IsAttachedToSource -> predicate
  Filter.CanHostSubject -> predicate
  Filter.IsToken -> predicate
  Filter.IsTapped -> predicate
  Filter.IsRingBearer -> predicate
  Filter.HasDesignation _ -> predicate
  -- Untouched: CR 612.1 swaps a subtype, a colour or a card type word, and this
  -- atom names none -- "an activated ability that isn't a mana ability" has no
  -- word inside it for Artificial Evolution to reach.
  Filter.HasNonManaActivatedAbility -> predicate
  -- Rewritten THROUGH the kind: CR 122.1b's keyword counter carries a keyword,
  -- and rule 612.1 reaches a word inside one exactly as it does in HasKeyword
  -- above. Every other kind names no word to swap.
  Filter.HasCounters kind -> Filter.HasCounters $ case kind of
    CounterKind.Keyword k -> CounterKind.Keyword (rewriteKeyword pairs k)
    CounterKind.PlusOnePlusOne -> kind
    CounterKind.MinusOneMinusOne -> kind
    CounterKind.Loyalty -> kind
    CounterKind.Lore -> kind
    CounterKind.Defense -> kind
    CounterKind.Time -> kind
    CounterKind.Fade -> kind
    CounterKind.Shield -> kind

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
  Keyword.Type.Reach -> keyword
  Keyword.Type.Shroud -> keyword
  Keyword.Type.Trample -> keyword
  Keyword.Type.TrampleOverPlaneswalkers -> keyword
  Keyword.Type.Vigilance -> keyword
  -- CR 702.21a states its cost as part of the keyword too, so CR 612.1 reaches it
  -- the same way -- a ward cost naming a basic land type is the unprinted case
  -- the arms below are also a fence for.
  Keyword.Type.Ward cost -> Keyword.Type.Ward (rewriteCost pairs cost)
  -- CR 702.33a, CR 702.34a, CR 702.37a and CR 702.42a: each states a cost as part
  -- of the keyword, so rewriteCost carries CR 612.1 into it.
  Keyword.Type.Kicker cost -> Keyword.Type.Kicker (rewriteCost pairs cost)
  Keyword.Type.Flashback cost -> Keyword.Type.Flashback (rewriteCost pairs cost)
  Keyword.Type.Fear -> keyword
  Keyword.Type.Intimidate -> keyword
  Keyword.Type.Morph (Morph.MkMorph cost variant) -> Keyword.Type.Morph (Morph.MkMorph (rewriteCost pairs cost) variant)
  Keyword.Type.Entwine cost -> Keyword.Type.Entwine (rewriteCost pairs cost)
  -- CR 702.45a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Bushido _ -> keyword
  -- CR 702.46a's N is a number and not a word, so CR 612.2 has nothing to swap
  -- HERE. "Spirit" is a word CR 612.2a does reach, but it is in the ability
  -- Pawl.Engine.Keyword.soulshift mints rather than in this value, so the swap
  -- arrives there instead (Pawl.Engine.Projection.mintedTriggeredAbilitiesOf).
  Keyword.Type.Soulshift _ -> keyword
  -- CR 702.54a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "+1/+1 counter" is the rule's own noun and no card prints it.
  Keyword.Type.Bloodthirst _ -> keyword
  -- CR 702.61a names no word CR 612.2 can swap: "mana ability" is CR 605.1a's
  -- own classification and "the stack" is a zone.
  Keyword.Type.SplitSecond -> keyword
  -- CR 702.77a states a cost, so rewriteCost reaches it as flashback's does. The
  -- N is a number and not a word, and "+1/+1 counter" is in the ability
  -- Pawl.Engine.Keyword.reinforce mints rather than in this value.
  Keyword.Type.Reinforce (Reinforce.MkReinforce n cost) -> Keyword.Type.Reinforce (Reinforce.MkReinforce n (rewriteCost pairs cost))
  -- CR 702.43a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "+1/+1 counter" is the rule's own noun and no card prints it.
  Keyword.Type.Modular _ -> keyword
  -- CR 702.63a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "time counter" is the rule's own noun and no card prints it.
  Keyword.Type.Vanishing _ -> keyword
  -- CR 702.32a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "fade counter" is in the replacement and the ability Pawl.Engine.Keyword mints
  -- rather than in this value.
  Keyword.Type.Fading _ -> keyword
  -- CR 702.68a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- the bonus is in the ability Pawl.Engine.Keyword.frenzy mints.
  Keyword.Type.Frenzy _ -> keyword
  Keyword.Type.Poisonous _ -> keyword
  Keyword.Type.Renown _ -> keyword
  -- CR 702.86a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Annihilator _ -> keyword
  Keyword.Type.Infect -> keyword
  Keyword.Type.Wither -> keyword
  Keyword.Type.Exalted -> keyword
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
  Keyword.Type.Dethrone -> keyword
  Keyword.Type.Outlast cost -> Keyword.Type.Outlast (rewriteCost pairs cost)
  Keyword.Type.Prowess -> keyword
  Keyword.Type.Menace -> keyword
  -- CR 702.73a names no word either: "every creature type" is CR 205.3m's
  -- whole family, so a CR 612.2 swap inside it has nothing to rewrite.
  Keyword.Type.Changeling -> keyword
  Keyword.Type.Devoid -> keyword
  -- CR 702.115a is payload-free, so it holds no word to swap: the library it
  -- names is "their" own, written into the rule rather than into the keyword.
  Keyword.Type.Ingest -> keyword
  -- CR 702.122a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Crew _ -> keyword
  Keyword.Type.Fabricate _ -> keyword
  Keyword.Type.Riot -> keyword
  Keyword.Type.Unleash -> keyword
  Keyword.Type.Daybound -> keyword
  Keyword.Type.Nightbound -> keyword
  -- CR 702.147a names no word CR 612.2 can swap: "end of combat" is the rules'
  -- own step and the ability it arms is written in Pawl.Engine.Keyword.
  Keyword.Type.Decayed -> keyword
  Keyword.Type.Toxic _ -> keyword
  -- CR 702.170a states a cost, so rewriteCost reaches it as flashback's does.
  Keyword.Type.Plot cost -> Keyword.Type.Plot (rewriteCost pairs cost)
  -- CR 702.143a states a cost too, so it is reached the same way.
  Keyword.Type.Foretell cost -> Keyword.Type.Foretell (rewriteCost pairs cost)
  -- CR 702.94a states a cost too, so it is reached the same way.
  Keyword.Type.Miracle cost -> Keyword.Type.Miracle (rewriteCost pairs cost)
  Keyword.Type.StartYourEngines -> keyword
  Keyword.Type.Persist -> keyword
  Keyword.Type.Undying -> keyword

-- CR 612.1's word swap inside a COST. CR 118.1 makes a cost "an action or payment
-- necessary to take another action", and the one on an activated ability is
-- printed in the text box that rule 612 reaches -- Dark Heart of the Wood's
-- "Sacrifice a Forest:" is the printing, and a Magical Hack naming Forest turns it
-- into "Sacrifice an Island:". CR 602.2a is why fixing it here is enough for the
-- payment: the ability on the stack "has the text of the ability that created
-- it", so it pays the cost this rewrite produced.
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

-- rewriteCost's per-component half. Five components carry a Filter and are the
-- five that descend; the rest name a number, or the object the cost is on, and
-- CR 612.2 finds no word in them to swap.
--
-- Of the five, only Sacrifice has a producer -- Dark Heart of the Wood. The
-- TapForTotalPower, DiscardCards, ExileCardsFromGraveyard and
-- ExileTopFromGraveyard arms are a regression fence: no printing pairs any of
-- them with a basic land type, so no test can falsify them. Magmatic Insight's
-- "a land card" comes closest and is still not one -- CR 612.2 swaps a SUBTYPE
-- word, and the land CARD TYPE is not one.
rewriteComponent :: [(Subtype.Subtype, Subtype.Subtype)] -> CostComponent.CostComponent Keyword.Type.Keyword -> CostComponent.CostComponent Keyword.Type.Keyword
rewriteComponent pairs component = case component of
  CostComponent.Sacrifice (Sacrifice.MkSacrifice n criterion) -> CostComponent.Sacrifice (Sacrifice.MkSacrifice n (rewrite pairs criterion))
  CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n criterion) -> CostComponent.TapForTotalPower (TapForTotalPower.MkTapForTotalPower n (rewrite pairs criterion))
  CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n criterion) -> CostComponent.ExileCardsFromGraveyard (ExileCardsFromGraveyard.MkExileCardsFromGraveyard n (rewrite pairs criterion))
  CostComponent.ExileTopFromGraveyard criterion -> CostComponent.ExileTopFromGraveyard (rewrite pairs criterion)
  CostComponent.DiscardCards (DiscardCards.MkDiscardCards n criterion) -> CostComponent.DiscardCards (DiscardCards.MkDiscardCards n (rewrite pairs criterion))
  CostComponent.TapThis -> component
  CostComponent.UntapThis -> component
  CostComponent.SacrificeThis -> component
  CostComponent.PayLife _ -> component
  CostComponent.PayLifeX -> component
  CostComponent.DiscardThis -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.PutPlusOneCountersOnThis _ -> component
  CostComponent.ExileThisFromGraveyard -> component

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
  Filter.ControlsMoreThanYou f -> Filter.ControlsMoreThanYou (bakeBound players f)
  Filter.ControlledByPlayer _ -> predicate
  -- Untouched: CR 603.2's slot is not the recipient an effect has reached, and no
  -- binding could answer this atom -- Pawl.Engine.Filter.Context carries it.
  Filter.ControlledByRecipient -> predicate
  Filter.HasCardType _ -> predicate
  Filter.HasSupertype _ -> predicate
  Filter.HasColor _ -> predicate
  Filter.HasSubtype _ -> predicate
  Filter.HasName _ -> predicate
  Filter.HasKeyword _ -> predicate
  Filter.HasKeywordFamily _ -> predicate
  Filter.PowerAtLeast _ -> predicate
  Filter.PowerAtMost _ -> predicate
  Filter.PowerLessThanSource -> predicate
  Filter.PowerGreaterThanSource -> predicate
  Filter.ManaValueAtMost _ -> predicate
  Filter.ManaValueIsEven -> predicate
  Filter.ControlledBy _ -> predicate
  Filter.ControlledByDefendingPlayer -> predicate
  Filter.OwnedBy _ -> predicate
  Filter.IsSource -> predicate
  -- Untouched for the reason IsControllerOfBound below is, and one step shorter:
  -- CR 603.2's binding map holds PLAYERS and this atom names a slot holding an
  -- OBJECT. Pawl.Engine.Filter.matches answers it as it stands.
  Filter.IsBound _ -> predicate
  Filter.SameNameAsBound _ -> predicate
  Filter.IsPlayer _ -> predicate
  -- Untouched: CR 603.2's binding map holds PLAYERS, and this atom names a slot
  -- holding an OBJECT -- there is nothing here to substitute.
  -- Pawl.Engine.Count.bakePerspective is where it is answered instead.
  Filter.IsControllerOfBound _ -> predicate
  Filter.IsAttacking -> predicate
  Filter.IsBlocking -> predicate
  Filter.IsBlocked -> predicate
  Filter.AttackedThisTurn -> predicate
  Filter.MilledThisTurn -> predicate
  Filter.IsAttachedToCreature -> predicate
  Filter.IsAttachedToPermanent -> predicate
  Filter.IsAttachedToSource -> predicate
  Filter.CanHostSubject -> predicate
  Filter.IsToken -> predicate
  Filter.IsTapped -> predicate
  Filter.IsRingBearer -> predicate
  Filter.HasDesignation _ -> predicate
  Filter.HasCounters _ -> predicate
  Filter.HasNonManaActivatedAbility -> predicate

-- The mana-value LITERALS a Filter compares against: every `n` in a
-- ManaValueAtMost atom inside it, at any depth.
--
-- CR 601.3a's lookahead is the one caller (Pawl.Engine.PlayerEffect
-- prohibitsCasting). Asking whether some choice of X could take a spell out of a
-- prohibition means asking one Filter at more than one mana value, and this is
-- what BOUNDS that search: ManaValueAtMost and ManaValueIsEven are the whole of
-- this language's mana-value vocabulary, so above every literal returned here the
-- only distinction a Filter can still draw is parity, and a sample running two
-- past the greatest literal has already seen every verdict the Filter can give.
--
-- Exhaustive rather than a catch-all, bakeBound's posture and for a sharper
-- reason: an atom reading the mana value some other way -- a multiple-of-three
-- test -- would break that argument, so it must break this build instead of
-- silently narrowing the search.
--
-- Polymorphic in the keyword, since no arm reads one.
manaValueThresholds :: Filter.Filter keyword -> [Integer]
manaValueThresholds predicate = case predicate of
  Filter.ManaValueAtMost n -> [n]
  Filter.And fs -> concatMap manaValueThresholds fs
  Filter.Or fs -> concatMap manaValueThresholds fs
  Filter.Not f -> manaValueThresholds f
  -- Descended into, which OVER-reports: the literals inside bound the mana value
  -- of the permanents being counted, never the candidate's own. Reporting them
  -- only widens CR 601.3a's sample, and the alternative -- an empty list -- would
  -- have to argue that no nested atom can ever matter, which is a claim about the
  -- inner filter rather than about this atom.
  Filter.ControlsMoreThanYou f -> manaValueThresholds f
  -- Reads the mana value and compares it against NO literal, so it bounds
  -- nothing: parity is what the sample's two-past-the-greatest tail is for.
  Filter.ManaValueIsEven -> []
  Filter.HasCardType _ -> []
  Filter.HasSupertype _ -> []
  Filter.HasColor _ -> []
  Filter.HasSubtype _ -> []
  Filter.HasName _ -> []
  Filter.HasKeyword _ -> []
  Filter.HasKeywordFamily _ -> []
  Filter.PowerAtLeast _ -> []
  Filter.PowerAtMost _ -> []
  Filter.PowerLessThanSource -> []
  Filter.PowerGreaterThanSource -> []
  Filter.ControlledBy _ -> []
  Filter.ControlledByDefendingPlayer -> []
  Filter.ControlledByBound _ -> []
  Filter.ControlledByPlayer _ -> []
  Filter.ControlledByRecipient -> []
  Filter.OwnedBy _ -> []
  Filter.IsSource -> []
  Filter.IsBound _ -> []
  Filter.SameNameAsBound _ -> []
  Filter.IsPlayer _ -> []
  Filter.IsControllerOfBound _ -> []
  Filter.IsAttacking -> []
  Filter.IsBlocking -> []
  Filter.IsBlocked -> []
  Filter.AttackedThisTurn -> []
  Filter.MilledThisTurn -> []
  Filter.IsAttachedToCreature -> []
  Filter.IsAttachedToPermanent -> []
  Filter.IsAttachedToSource -> []
  Filter.CanHostSubject -> []
  Filter.IsToken -> []
  Filter.IsTapped -> []
  Filter.IsRingBearer -> []
  Filter.HasDesignation _ -> []
  Filter.HasCounters _ -> []
  Filter.HasNonManaActivatedAbility -> []

-- CR 701.23b vs CR 701.23d: does this predicate state a QUALITY? A search whose
-- filter states one may find fewer cards than it asks for, or none, even when the
-- zone holds them (701.23b); a search "simply for a quantity of cards" -- "a
-- card", which is the whole of Extract's filter -- must find that many if the
-- zone can supply them (701.23d). Pawl.Engine.Resolve's Search arm is the caller,
-- and the answer is the difference between an answer of "nothing" being honoured
-- and being overridden.
--
-- Derived rather than stored on the opcode: the two are not independent -- the
-- rule reads the search's own description of what it looks for -- so a flag
-- beside the Filter could only ever disagree with it.
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
  Filter.ControlsMoreThanYou _ -> True
  Filter.ManaValueAtMost _ -> True
  Filter.ManaValueIsEven -> True
  Filter.HasCardType _ -> True
  Filter.HasSupertype _ -> True
  Filter.HasColor _ -> True
  Filter.HasSubtype _ -> True
  -- CR 701.23b's "stated quality" at its sharpest -- a named card is the most
  -- specific description a search can give -- so the searcher may decline to
  -- find one that is there, and CR 701.23d's "must find" does not apply.
  Filter.HasName _ -> True
  Filter.HasKeyword _ -> True
  Filter.HasKeywordFamily _ -> True
  Filter.PowerAtLeast _ -> True
  Filter.PowerAtMost _ -> True
  Filter.PowerLessThanSource -> True
  Filter.PowerGreaterThanSource -> True
  Filter.ControlledBy _ -> True
  Filter.ControlledByDefendingPlayer -> True
  Filter.ControlledByBound _ -> True
  Filter.ControlledByPlayer _ -> True
  Filter.ControlledByRecipient -> True
  Filter.OwnedBy _ -> True
  Filter.IsSource -> True
  Filter.IsBound _ -> True
  Filter.SameNameAsBound _ -> True
  Filter.IsPlayer _ -> True
  Filter.IsControllerOfBound _ -> True
  Filter.IsAttacking -> True
  Filter.IsBlocking -> True
  Filter.IsBlocked -> True
  Filter.AttackedThisTurn -> True
  Filter.MilledThisTurn -> True
  Filter.IsAttachedToCreature -> True
  Filter.IsAttachedToPermanent -> True
  Filter.IsAttachedToSource -> True
  Filter.CanHostSubject -> True
  Filter.IsToken -> True
  Filter.IsTapped -> True
  Filter.IsRingBearer -> True
  Filter.HasDesignation _ -> True
  Filter.HasCounters _ -> True
  Filter.HasNonManaActivatedAbility -> True

-- The slots a Filter READS -- today exactly the ControlledByBound atoms in it.
-- Pawl.Engine.Resolve.modeSlots folds this over a mode's target slots, which is
-- what makes the card dataflow lint see a slot named in a FILTER rather than in
-- an effect: a card reading "that player" under a condition that never binds one
-- is then a failing test rather than a slot that silently admits nothing.
--
-- The same descent `bakeBound` makes, and deliberately so: a position that
-- function does not bake is a position this one must not report as read, or the
-- lint would bless an atom the engine can never answer.
boundSlots :: Filter.Filter Keyword.Type.Keyword -> Set.Set SlotName.SlotName
boundSlots predicate = case predicate of
  Filter.ControlledByBound slot -> Set.singleton slot
  -- Reported although `bakeBound` above leaves it standing, which the pairing
  -- this function's comment states would otherwise forbid. What the pairing is
  -- for is that a reported slot be ANSWERABLE, and this one is -- one module
  -- over, at Pawl.Engine.Count.bakePerspective, which holds the board a
  -- controller has to be projected off.
  Filter.IsControllerOfBound slot -> Set.singleton slot
  -- Reported although `bakeBound` leaves it standing too, and answerable in the
  -- same sense the atom above is -- here rather than one module over, off the
  -- Context `matches` is already handed.
  Filter.IsBound slot -> Set.singleton slot
  -- Reported for the atom above's reason, and answerable in the same place: it
  -- reads the Context too, one field over.
  Filter.SameNameAsBound slot -> Set.singleton slot
  Filter.And fs -> foldMap boundSlots fs
  Filter.Or fs -> foldMap boundSlots fs
  Filter.Not f -> boundSlots f
  -- Descended into because `bakeBound` descends into it, which is the pairing this
  -- function's comment above insists on. The catch-all below would have absorbed
  -- it silently, this being the first atom to carry a Filter DIRECTLY -- a
  -- keyword's own filter (CR 702.29e) is out of both functions' reach alike, so
  -- the pairing holds there by both sides declining.
  Filter.ControlsMoreThanYou f -> boundSlots f
  _ -> Set.empty
