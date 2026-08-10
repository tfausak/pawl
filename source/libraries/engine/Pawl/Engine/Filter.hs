module Pawl.Engine.Filter where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword.Type
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

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
  { cardTypes :: Set.Set CardType.CardType,
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
    -- CR 608.2i: was this candidate declared as an attacker earlier this turn?
    -- Unlike `attacking` not even a present state: it is a look-back read of the
    -- turn-scoped GameEvent log.
    --
    -- LAZY, like `attachedToCreature` below but for a plainer reason: filling it
    -- folds the whole turn's event log, and nothing forces it unless a Filter
    -- actually contains AttackedThisTurn. That is a cost argument rather than
    -- the recursion hazard the next field records.
    attackedThisTurn :: Bool,
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
    -- the projection that is asking. That is the same laziness accident
    -- Projection.affects records for `perspective` (#197), and it is a fact about
    -- the pool's card data rather than a guarantee this record enforces.
    attachedToCreature :: Bool,
    -- CR 303.4: is this candidate attached to a PERMANENT right now? Read from
    -- Object.attachedTo alone -- whether the attachment names an object rather
    -- than a player, which is what Recipient.objectOf asks. Unlike
    -- `attachedToCreature` this reads no second projection, so it needs no
    -- laziness argument.
    attachedToPermanent :: Bool,
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
    -- CR 702.112b: does this candidate have the RENOWNED designation? Read
    -- straight off Object.renowned, for ringBearerFor's reason -- rule 702.112b
    -- makes it a designation rather than a characteristic, so no projection
    -- writes it -- and a Bool rather than a Maybe because the rule names no
    -- player.
    --
    -- False for every candidate with no object to read it off: a printed card off
    -- the battlefield, a player, an event snapshot -- the vacuous posture `tapped`
    -- and `token` already take.
    renowned :: Bool,
    -- CR 701.37b: does this candidate have the MONSTROUS designation? Read off
    -- Object.monstrous, and False where there is no object to read it off, both
    -- for `renowned` above's reasons -- the two rules word their designations
    -- the same way.
    monstrous :: Bool
  }
  deriving (Eq, Show)

-- The view of a PLAYER candidate: no card types, no colours, no controller --
-- a player is not an object (CR 109.1) and has none of those. Only the player's
-- own identity is answerable, which is exactly what IsPlayer asks.
playerView :: PlayerId.PlayerId -> View
playerView pid =
  MkView
    { cardTypes = Set.empty,
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
      -- CR 506.3 again: a player was never declared as an attacker either.
      attackedThisTurn = False,
      -- CR 303.4b: a player an Aura is attached to is ENCHANTED by it; the
      -- player is not itself attached to anything, because Object.attachedTo is
      -- a field of the ATTACHED permanent, and a player is not one.
      attachedToCreature = False,
      -- CR 303.4 again, for the same reason.
      attachedToPermanent = False,
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
      -- CR 702.112b: "only permanents can be or become renowned", and a player is
      -- not one.
      renowned = False,
      monstrous = False
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
    -- Pawl.Engine.Target.admittedGiven for a target slot, and
    -- Pawl.Engine.Event.matchesTrigger for CR 702.149a's trigger condition.
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
    -- The objects the surrounding resolution's LEGAL slots name, for
    -- Quantity.AgainstSlot to aim an evaluation at one (CR 608.2b keeps an
    -- illegal slot out). No Filter atom reads it -- it rides here because this
    -- record is already the evaluation context every Quantity is handed, and a
    -- slot map is exactly the part of a resolution the evaluator cannot derive.
    --
    -- EMPTY everywhere but a resolution, which is the honest answer rather than a
    -- forgotten filler: outside one there are no slots. Pawl.Engine.Resolve's
    -- effectContext is the sole non-empty producer.
    slotObjects :: Map.Map SlotName.SlotName ObjectId.ObjectId
  }
  deriving (Eq, Show)

-- A Context for every match whose Filter cannot name a context-relative atom --
-- that is, every match but a target slot's and CR 702.149a's trigger condition.
-- The source-power atoms reach a card only through Pawl.Engine.Keyword's own
-- mentor and training, and CR 702.39a's defending-player atom only through
-- provoke; Pawl.CardSpec's lints keep all three out of card data, so no other
-- position can read the Nothings this leaves.
contextFor :: Maybe PlayerId.PlayerId -> Maybe ObjectId.ObjectId -> Context
contextFor p s = MkContext {perspective = p, source = s, sourcePower = Nothing, defendingPlayer = Nothing, slotObjects = Map.empty}

-- contextFor with the resolution's slot objects supplied. The one caller is
-- Pawl.Engine.Resolve.effectContext; see slotObjects above.
contextWithSlots :: Maybe PlayerId.PlayerId -> Maybe ObjectId.ObjectId -> Map.Map SlotName.SlotName ObjectId.ObjectId -> Context
contextWithSlots p s m = (contextFor p s) {slotObjects = m}

-- contextFor with the source's power supplied. Kept lazy at the call site, since
-- the field is: a Filter that never names the atom pays for no projection.
--
-- The defending player stays Nothing: this is CR 702.149a's TRIGGER match, and
-- rule 702.39a's atom lives only in a target slot.
contextComparingPower :: Maybe PlayerId.PlayerId -> ObjectId.ObjectId -> Maybe Integer -> Context
contextComparingPower p s n = MkContext {perspective = p, source = Just s, sourcePower = n, defendingPlayer = Nothing, slotObjects = Map.empty}

-- The one generic matcher. A pure fold over the Filter tree; it never inspects
-- which effect produced the Filter. Identity checks like IsSource consult the
-- supplied Context, not information baked into the predicate.
matches :: Context -> View -> Filter.Filter Keyword.Type.Keyword -> Bool
matches context view predicate = case predicate of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.HasSubtype s -> Set.member s (subtypes view)
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
  -- CR 115.1's "target opponent". Same "every other player is an opponent"
  -- reading the ControlledBy arm above argues for, and wrong for the same one
  -- case (CR 102.3's teams, #175). Vacuously False for an object candidate,
  -- which has no playerIdentity, and for a match with no perspective.
  Filter.IsPlayer relation -> case (playerIdentity view, perspective context) of
    (Just candidate, Just you) -> case relation of
      PlayerRelation.You -> candidate == you
      PlayerRelation.Opponent -> candidate /= you
    _ -> False
  -- CR 508.1k: a creature stays attacking until it is removed from combat or the
  -- combat phase ends, so this is a live read of the combat record, never a stamp
  -- on the object.
  Filter.IsAttacking -> attacking view
  -- CR 509.1g: the same live read IsAttacking is, off the other map. Never the
  -- question Pawl.Engine.Combat.isBlocked asks: CR 509.1h keeps an attacker
  -- blocked after every creature blocking it has gone, so this can be False for
  -- everything while that is still True.
  Filter.IsBlocking -> blocking view
  -- CR 608.2i: a look-back read of the turn's event log. Unlike IsAttacking it
  -- cannot stop being true within a turn -- nothing removes a GameEvent -- so a
  -- creature removed from combat (CR 506.4) still attacked, which is what
  -- Relentless Assault's "creatures that attacked this turn" means.
  Filter.AttackedThisTurn -> attackedThisTurn view
  -- CR 701.3a: a live read of Object.attachedTo and the host's projected types,
  -- never a stamp on the candidate -- an Aura whose host stops being a creature
  -- stops matching, and CR 704.5m buries it on the next state-based-action pass.
  Filter.IsAttachedToCreature -> attachedToCreature view
  -- CR 303.4: a live read of Object.attachedTo, and of nothing else -- whether the
  -- attachment names an object rather than a player. An Aura buried by CR 704.5m
  -- stops matching because it stops being attached, never because a stamp was
  -- cleared.
  Filter.IsAttachedToPermanent -> attachedToPermanent view
  -- CR 701.3a: a live read of the legality of the attach this match is framing,
  -- computed by the caller that knows what is moving. Vacuously False outside one.
  Filter.CanHostSubject -> canHostSubject view
  -- CR 111.6: a token isn't a card. A live read of what the object is
  -- represented by (Object.source), never a stamp on the candidate -- and unlike
  -- the two arms above it cannot change while the game runs, because CR 111.3
  -- makes a token's characteristics equivalent to a card's.
  Filter.IsToken -> token view
  Filter.IsTapped -> tapped view
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
  -- CR 702.112b's designation, asked of the CANDIDATE. A live read of
  -- Object.renowned, never a stamp on the candidate: the rule ends the
  -- designation when the permanent leaves the battlefield, and CR 400.7's new
  -- incarnation simply arrives without it. Asks nothing of the perspective,
  -- unlike the arm above -- the designation belongs to no player.
  Filter.IsRenowned -> renowned view
  -- CR 122.1, asked of the CANDIDATE: has it one or more counters of the kind?
  -- IsRenowned's live read, of counters instead of a designation: CR 400.7's new
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
  -- CR 702.14a: a keyword can hold a land-type word too, so "creature with
  -- swampwalk" is text a swap reaches exactly as "creature that's a Swamp" is.
  -- rewriteKeyword below is the descent, shared with the two sites that rewrite
  -- a keyword rather than a filter over one.
  Filter.HasKeyword k -> Filter.HasKeyword (rewriteKeyword pairs k)
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
  Filter.ControlledBy _ -> predicate
  -- Untouched for ControlledBy's reason.
  Filter.ControlledByDefendingPlayer -> predicate
  -- Untouched for ControlledBy's reason: CR 612.1 swaps a WORD in the text, and
  -- this atom names a player relation rather than a subtype.
  Filter.OwnedBy _ -> predicate
  Filter.IsSource -> predicate
  Filter.IsPlayer _ -> predicate
  Filter.IsAttacking -> predicate
  Filter.IsBlocking -> predicate
  Filter.AttackedThisTurn -> predicate
  Filter.IsAttachedToCreature -> predicate
  Filter.IsAttachedToPermanent -> predicate
  Filter.CanHostSubject -> predicate
  Filter.IsToken -> predicate
  Filter.IsTapped -> predicate
  Filter.IsRingBearer -> predicate
  Filter.IsRenowned -> predicate
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
-- The Cost that cycling, flashback, morph and entwine carry goes through
-- rewriteCost below, for CR 612.1's own reason: rule 702 states those costs as
-- part of the keyword, so they are printed in the text box exactly as an
-- activated ability's activation cost is. No printing pairs one of those costs
-- with a basic land type, so those four arms are a regression fence rather than
-- a proven path -- Pawl.ActivateSpec's Dark Heart of the Wood is what proves
-- rewriteCost itself.
rewriteKeyword :: [(Subtype.Subtype, Subtype.Subtype)] -> Keyword.Type.Keyword -> Keyword.Type.Keyword
rewriteKeyword pairs keyword = case keyword of
  -- CR 702.14a's "[type]walk".
  Keyword.Type.Landwalk criterion -> Keyword.Type.Landwalk (rewrite pairs criterion)
  -- CR 702.29e's "[Type]cycling", rule 702's other "[type]": "usually a subtype
  -- (as in 'mountaincycling')", so it holds a basic land type exactly as
  -- swampwalk does.
  Keyword.Type.Cycling cost criterion -> Keyword.Type.Cycling (rewriteCost pairs cost) (fmap (rewrite pairs) criterion)
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
  -- CR 702.34a, CR 702.37a and CR 702.42a: each states a cost as part of the
  -- keyword, so rewriteCost carries CR 612.1 into it.
  Keyword.Type.Flashback cost -> Keyword.Type.Flashback (rewriteCost pairs cost)
  Keyword.Type.Fear -> keyword
  Keyword.Type.Intimidate -> keyword
  Keyword.Type.Morph cost variant -> Keyword.Type.Morph (rewriteCost pairs cost) variant
  Keyword.Type.Entwine cost -> Keyword.Type.Entwine (rewriteCost pairs cost)
  -- CR 702.45a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Bushido _ -> keyword
  -- CR 702.46a's N is a number and not a word, so CR 612.2 has nothing to swap
  -- HERE. "Spirit" is a word CR 612.2a does reach, but it is in the ability
  -- Pawl.Engine.Keyword.soulshift mints rather than in this value (#1197).
  Keyword.Type.Soulshift _ -> keyword
  -- CR 702.61a names no word CR 612.2 can swap: "mana ability" is CR 605.1a's
  -- own classification and "the stack" is a zone.
  Keyword.Type.SplitSecond -> keyword
  -- CR 702.77a states a cost, so rewriteCost reaches it as flashback's does. The
  -- N is a number and not a word, and "+1/+1 counter" is in the ability
  -- Pawl.Engine.Keyword.reinforce mints rather than in this value.
  Keyword.Type.Reinforce n cost -> Keyword.Type.Reinforce n (rewriteCost pairs cost)
  -- CR 702.43a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "+1/+1 counter" is the rule's own noun and no card prints it.
  Keyword.Type.Modular _ -> keyword
  -- CR 702.63a's N is a number and not a word, so CR 612.2 has nothing to swap;
  -- "time counter" is the rule's own noun and no card prints it.
  Keyword.Type.Vanishing _ -> keyword
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
  -- Pawl.Engine.Keyword.afterlife mints rather than in this value (#1197).
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
  -- CR 702.122a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Crew _ -> keyword
  Keyword.Type.Riot -> keyword
  Keyword.Type.Unleash -> keyword
  Keyword.Type.Daybound -> keyword
  Keyword.Type.Nightbound -> keyword
  -- CR 702.147a names no word CR 612.2 can swap: "end of combat" is the rules'
  -- own step and the ability it arms is written in Pawl.Engine.Keyword.
  Keyword.Type.Decayed -> keyword
  Keyword.Type.Toxic _ -> keyword
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

-- rewriteCost's per-component half. Four components carry a Filter and are the
-- four that descend; the rest name a number, or the object the cost is on, and
-- CR 612.2 finds no word in them to swap.
--
-- Of the four, only Sacrifice has a producer -- Dark Heart of the Wood. The
-- TapForTotalPower, ExileCardsFromGraveyard and ExileTopFromGraveyard arms are a
-- regression fence: no printing pairs any of them with a basic land type, so no
-- test can falsify them.
rewriteComponent :: [(Subtype.Subtype, Subtype.Subtype)] -> CostComponent.CostComponent Keyword.Type.Keyword -> CostComponent.CostComponent Keyword.Type.Keyword
rewriteComponent pairs component = case component of
  CostComponent.Sacrifice n criterion -> CostComponent.Sacrifice n (rewrite pairs criterion)
  CostComponent.TapForTotalPower n criterion -> CostComponent.TapForTotalPower n (rewrite pairs criterion)
  CostComponent.ExileCardsFromGraveyard n criterion -> CostComponent.ExileCardsFromGraveyard n (rewrite pairs criterion)
  CostComponent.ExileTopFromGraveyard criterion -> CostComponent.ExileTopFromGraveyard (rewrite pairs criterion)
  CostComponent.TapThis -> component
  CostComponent.UntapThis -> component
  CostComponent.SacrificeThis -> component
  CostComponent.PayLife _ -> component
  CostComponent.DiscardCards _ -> component
  CostComponent.DiscardThis -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.PutPlusOneCountersOnThis _ -> component
  CostComponent.ExileThisFromGraveyard -> component
