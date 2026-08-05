module Pawl.Engine.Filter where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.Keyword as Keyword
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
    -- projection's Map Keyword Natural, because HasKeyword asks membership and
    -- nothing else. Read from the PROJECTION on the battlefield and from the
    -- printed card off it, so a creature that gains flying at layer 6 matches and
    -- a Humility'd one (CR 613.1f) does not.
    keywords :: Set.Set Keyword.Keyword,
    power :: Maybe Integer,
    -- CR 202.3: the candidate's mana value, computed from its printed mana cost
    -- (CR 202.3a gives a costless object 0). Unlike `power` this is NOT Nothing
    -- off the battlefield -- a mana cost is printed on the card and rule 202.3
    -- names no zone -- which is what lets ManaValueAtMost filter a graveyard.
    -- Nothing only where there is no card to read: a player view.
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
    -- candidate's projected characteristics. Pawl.Engine.Resolve's AttachTarget arm is
    -- the only site that fills it, from Resolve.attachmentFor -- the same function
    -- that performs the move, so the offer and the move cannot disagree.
    --
    -- False everywhere else, and that is not a lost distinction: outside an attach
    -- there is no subject for the question to be about. A Filter that named the
    -- atom from any other position would read that vacuous False, so no card is
    -- allowed to -- Pawl.CardSpec rejects it in every Filter position a card has
    -- but this one's. Widening the subject to somewhere every evaluation can see
    -- it is #572.
    canHostSubject :: Bool,
    -- CR 111.1 / 111.6: is this candidate a token rather than a card? Read from
    -- Object.source (Pawl.Engine.Game.isToken), never from a projection -- CR 111.3 makes
    -- a token's effect-defined characteristics equivalent to printed ones, so no
    -- characteristic axis distinguishes the two and no CR 613 layer can change the
    -- answer. False for every candidate with no object behind it.
    token :: Bool
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
      token = False
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
matches :: Context -> View -> Filter.Filter Keyword.Keyword -> Bool
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
  Filter.PowerAtLeast n -> case power view of
    Nothing -> False
    Just p -> p >= n
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
-- HasSubtype is the only atom REWRITTEN here. Most of the rest name a card type,
-- a supertype, a colour, a number, a relation or a status, none of which CR 612's
-- word swap reaches; HasKeyword is the one exception, and its own arm below says
-- why it is left alone anyway. Written out exhaustively rather than with a
-- catch-all, so a later atom that can carry a subtype fails to compile here
-- instead of silently going unrewritten.
--
-- CR 612.2's family gate is not restated on the HasSubtype arm, for the reason
-- Pawl.Engine.Projection's type-line half gives: a HasSubtype atom may name a
-- word of any family, so the family the word is used as IS the family the word
-- belongs to, and the exact `lookup` already asks CR 612.2's question.
rewrite :: [(Subtype.Subtype, Subtype.Subtype)] -> Filter.Filter Keyword.Keyword -> Filter.Filter Keyword.Keyword
rewrite pairs predicate = case predicate of
  Filter.HasSubtype s -> Filter.HasSubtype (Maybe.fromMaybe s (lookup s pairs))
  Filter.And fs -> Filter.And (fmap (rewrite pairs) fs)
  Filter.Or fs -> Filter.Or (fmap (rewrite pairs) fs)
  Filter.Not f -> Filter.Not (rewrite pairs f)
  Filter.HasCardType _ -> predicate
  Filter.HasSupertype _ -> predicate
  Filter.HasColor _ -> predicate
  -- Not rewritten, and CR 702.14a is why it is a live question rather than an
  -- obvious no: landwalk carries a criterion naming a land type
  -- (Keyword.Landwalk (HasSubtype Swamp)), so a text-changing effect could in
  -- principle reach the word inside this atom, and since that payload is a
  -- Filter (#499) this very function would be the fix.
  --
  -- Still nothing to rewrite: the pool's one granted landwalk (Vectis Gloves)
  -- has a HasCardType Artifact criterion, which names no subtype word for CR
  -- 612.1's swap to reach, and nothing in the pool filters by a landwalk either.
  -- Note the separate half: Projection.project's ChangeSubtypeWord arm rewrites
  -- PC.subtypes and never PC.keywords, so a Magical Hack on Bog Wraith would not
  -- make islandwalk even if this arm recursed. Matches
  -- Pawl.Engine.Projection.rewriteModification's identical silence on
  -- Modification.GainKeyword (#523).
  Filter.HasKeyword _ -> predicate
  Filter.PowerAtLeast _ -> predicate
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
