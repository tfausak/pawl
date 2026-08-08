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
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Supertype as Supertype

-- The characteristics a Filter atom consults. Supplied by the projection on the
-- battlefield/stack and by the printed card off the battlefield (both builders
-- live in Pawl.Engine.Projection), or by `playerView` below when the candidate is a
-- player rather than an object. `power` and `controller` are Nothing off the
-- battlefield -- a card in a library has neither under the rules that matter here
-- -- so PowerAtLeast / ControlledBy are vacuously False there, which no search
-- filter uses.
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
    -- The one field whose answer depends on something OTHER than the candidate,
    -- which is why it lives here rather than in Context: Context carries no game
    -- state, and this needs both the subject's enchant ability (CR 702.5a) and the
    -- candidate's projected characteristics. Pawl.Engine.Attach.hostsFor is the only
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
    counters :: Map.Map CounterKind.CounterKind Natural.Natural,
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
    ringBearerFor :: Maybe PlayerId.PlayerId
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
      -- CR 202.3 reads a mana cost, which is printed on an OBJECT (CR 202.1); a
      -- player has none.
      manaValue = Nothing,
      controller = Nothing,
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
      ringBearerFor = Nothing
    }

-- The perspective the match is relative to: who counts as "you" (CR 109.5), and
-- which object the surrounding effect comes from. Both are Nothing when no
-- player and no source frame the match (an off-battlefield search).
data Context = MkContext
  { perspective :: Maybe PlayerId.PlayerId,
    source :: Maybe ObjectId.ObjectId
  }
  deriving (Eq, Show)

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
  Filter.ManaValueAtMost _ -> predicate
  Filter.ControlledBy _ -> predicate
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
  Keyword.Type.Phasing -> keyword
  Keyword.Type.Aftermath -> keyword
  Keyword.Type.Flying -> keyword
  Keyword.Type.Haste -> keyword
  Keyword.Type.Indestructible -> keyword
  Keyword.Type.Lifelink -> keyword
  Keyword.Type.Reach -> keyword
  Keyword.Type.Shroud -> keyword
  Keyword.Type.Trample -> keyword
  Keyword.Type.Vigilance -> keyword
  -- CR 702.34a, CR 702.37a and CR 702.42a: each states a cost as part of the
  -- keyword, so rewriteCost carries CR 612.1 into it.
  Keyword.Type.Flashback cost -> Keyword.Type.Flashback (rewriteCost pairs cost)
  Keyword.Type.Fear -> keyword
  Keyword.Type.Morph cost variant -> Keyword.Type.Morph (rewriteCost pairs cost) variant
  Keyword.Type.Entwine cost -> Keyword.Type.Entwine (rewriteCost pairs cost)
  Keyword.Type.Poisonous _ -> keyword
  -- CR 702.86a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Annihilator _ -> keyword
  Keyword.Type.Infect -> keyword
  Keyword.Type.BattleCry -> keyword
  Keyword.Type.Menace -> keyword
  Keyword.Type.Devoid -> keyword
  -- CR 702.122a's N is a number and not a word, so CR 612.2 has nothing to swap.
  Keyword.Type.Crew _ -> keyword
  Keyword.Type.Riot -> keyword
  Keyword.Type.Daybound -> keyword
  Keyword.Type.Nightbound -> keyword
  Keyword.Type.Toxic _ -> keyword
  Keyword.Type.StartYourEngines -> keyword

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

-- rewriteCost's per-component half. Three components carry a Filter and are the
-- three that descend; the rest name a number, or the object the cost is on, and
-- CR 612.2 finds no word in them to swap.
--
-- Of the three, only Sacrifice has a producer -- Dark Heart of the Wood.
-- TapForTotalPower's and ExileCardsFromGraveyard's arms are a regression fence:
-- no printing pairs either with a basic land type, so no test can falsify them.
rewriteComponent :: [(Subtype.Subtype, Subtype.Subtype)] -> CostComponent.CostComponent Keyword.Type.Keyword -> CostComponent.CostComponent Keyword.Type.Keyword
rewriteComponent pairs component = case component of
  CostComponent.Sacrifice n criterion -> CostComponent.Sacrifice n (rewrite pairs criterion)
  CostComponent.TapForTotalPower n criterion -> CostComponent.TapForTotalPower n (rewrite pairs criterion)
  CostComponent.ExileCardsFromGraveyard n criterion -> CostComponent.ExileCardsFromGraveyard n (rewrite pairs criterion)
  CostComponent.TapThis -> component
  CostComponent.UntapThis -> component
  CostComponent.SacrificeThis -> component
  CostComponent.PayLife _ -> component
  CostComponent.DiscardCards _ -> component
  CostComponent.DiscardThis -> component
  CostComponent.PayEnergy _ -> component
  CostComponent.AddLoyaltyToThis _ -> component
  CostComponent.RemoveLoyaltyFromThis _ -> component
  CostComponent.ExileThisFromGraveyard -> component
