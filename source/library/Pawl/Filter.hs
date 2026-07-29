module Pawl.Filter where

import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Filter as Filter
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Supertype as Supertype

-- The characteristics a Filter atom consults. Supplied by the projection on the
-- battlefield/stack and by the printed card off the battlefield (both builders
-- live in Pawl.Projection), or by `playerView` below when the candidate is a
-- player rather than an object. `power` and `controller` are Nothing off the
-- battlefield -- a card in a library has neither under the rules that matter here
-- -- so PowerAtLeast / ControlledBy are vacuously False there, which no search
-- filter uses.
data View = MkView
  { cardTypes :: Set.Set CardType.CardType,
    supertypes :: Set.Set Supertype.Supertype,
    colors :: Set.Set Color.Color,
    subtypes :: Set.Set Subtype.Subtype,
    power :: Maybe Integer,
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
    -- GameState.combat rather than from a projection -- see
    -- Pawl.Projection.viewOfCharacteristics. False for every candidate with no
    -- combat status to read: a printed card off the battlefield, a player, and an
    -- event snapshot -- the same vacuous posture power and controller take.
    attacking :: Bool,
    -- CR 608.2i: was this candidate declared as an attacker earlier this turn?
    -- Not a characteristic either, and unlike `attacking` not even a present
    -- state: it is a look-back read of the turn-scoped GameEvent log, which
    -- CR 608.2i is the rule for -- see Pawl.Projection.viewOfCharacteristics.
    -- False for every candidate with no history to read: a printed card off the
    -- battlefield, a player, and an event snapshot -- the vacuous posture
    -- `attacking` takes.
    --
    -- LAZY, like `attachedToCreature` below but for a plainer reason: filling it
    -- folds the whole turn's event log, and nothing forces it unless a Filter
    -- actually contains AttackedThisTurn. That is a cost argument rather than
    -- the recursion hazard the next field records.
    attackedThisTurn :: Bool,
    -- CR 701.3a: is this candidate attached to a CREATURE right now? Not a
    -- characteristic either (CR 109.3 names "what an Aura enchants" among the
    -- things that are not one), so it is read from Object.attachedTo plus the
    -- HOST's projected card types rather than from the candidate's own
    -- projection -- see Pawl.Projection.viewOfCharacteristics. False for every
    -- candidate with no attachment to read: a printed card off the battlefield,
    -- a player, and an event snapshot -- the vacuous posture `attacking` takes.
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
    -- CR 111.1 / 111.6: is this candidate a token rather than a card? Read from
    -- Object.source (Pawl.Game.isToken), never from a projection -- CR 111.3 makes
    -- a token's effect-defined characteristics equivalent to printed ones, so no
    -- characteristic axis distinguishes the two and no CR 613 layer can change the
    -- answer. False for every candidate with no object behind it: a printed card
    -- off the battlefield, a player, and an event snapshot -- the same vacuous
    -- posture `attacking` and `attachedToCreature` take.
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
      power = Nothing,
      controller = Nothing,
      identity = Nothing,
      playerIdentity = Just pid,
      -- CR 506.3: only a creature can attack, and a player is not one.
      attacking = False,
      -- CR 506.3 again: a player was never declared as an attacker either.
      attackedThisTurn = False,
      -- CR 303.4b: a player an Aura is attached to is ENCHANTED by it; the
      -- player is not itself attached to anything (Object.attachedTo runs the
      -- other way and names an object, #190).
      attachedToCreature = False,
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
matches :: Context -> View -> Filter.Filter -> Bool
matches context view predicate = case predicate of
  Filter.HasCardType t -> Set.member t (cardTypes view)
  Filter.HasSupertype s -> Set.member s (supertypes view)
  Filter.HasColor c -> Set.member c (colors view)
  Filter.HasSubtype s -> Set.member s (subtypes view)
  Filter.PowerAtLeast n -> case power view of
    Nothing -> False
    Just p -> p >= n
  -- Every other player is an Opponent by construction: CR 806.1 has a
  -- free-for-all's players compete as individuals against each other, and CR
  -- 102.2 says the same for two players -- one predicate, `c /= p`, serves
  -- both. CR 102.3's teams are the ONE reading it is wrong for, and pawl has
  -- none to express (#175). Unlike Pawl.Count.playersFor, which folds a player
  -- SET, this arm tests one candidate `View` at a time, so there is no set
  -- here to get the size of wrong. `controller view == Nothing` off the
  -- battlefield is already covered by View's own haddock (vacuously False,
  -- the same posture PowerAtLeast takes).
  --
  -- Pinned at three seats by ResolveSpec's "CR 806.1 at three seats a
  -- ControlledBy Opponent pool spans BOTH opponents' creatures".
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
  -- CR 508.1k: "it remains an attacking creature until it's removed from combat
  -- or the combat phase ends, whichever comes first" -- so this is a live read of
  -- the combat record, never a stamp on the object.
  Filter.IsAttacking -> attacking view
  -- CR 608.2i: a look-back read of the turn's event log. Unlike IsAttacking it
  -- cannot stop being true within a turn -- nothing removes a GameEvent -- so a
  -- creature removed from combat (CR 506.4) still attacked, which is what
  -- Relentless Assault's "creatures that attacked this turn" means.
  Filter.AttackedThisTurn -> attackedThisTurn view
  -- CR 701.3a: a live read of Object.attachedTo and the host's projected types,
  -- never a stamp on the candidate -- an Aura whose host stops being a creature
  -- stops matching, and CR 704.5m buries it on the next state-based-action pass.
  Filter.IsAttachedToCreature -> attachedToCreature view
  -- CR 111.6: "A token isn't a card." A live read of what the object is
  -- represented by (Object.source), never a stamp on the candidate -- and unlike
  -- the two arms above it cannot change while the game runs, because CR 111.3
  -- makes a token's characteristics equivalent to a card's rather than a
  -- different kind of thing that some effect could convert.
  Filter.IsToken -> token view
  Filter.And fs -> all (matches context view) fs
  Filter.Or fs -> any (matches context view) fs
  Filter.Not f -> not (matches context view f)

-- CR 612.1: swap basic-land-type words wherever they appear in a Filter. A
-- text-changing effect "can apply to any words or symbols printed on that
-- object", and a Filter carried by an effect is part of that text -- so this is
-- the shape Pawl.Projection.rewriteModification already has, for the type THIS
-- module owns. Pawl.Resolve threads one call per Filter-carrying effect arm
-- rather than learning what is inside each one.
--
-- HasSubtype is the only atom that can carry a basic land type (CR 205.3i); the
-- rest name a card type, a supertype, a colour, a number, a relation or a
-- status, none of which CR 612's word swap reaches. Written out exhaustively
-- rather than with a catch-all, so a later atom that CAN carry one fails to
-- compile here instead of silently going unrewritten.
rewrite :: [(Subtype.Subtype, Subtype.Subtype)] -> Filter.Filter -> Filter.Filter
rewrite pairs predicate = case predicate of
  Filter.HasSubtype s -> Filter.HasSubtype (Maybe.fromMaybe s (lookup s pairs))
  Filter.And fs -> Filter.And (fmap (rewrite pairs) fs)
  Filter.Or fs -> Filter.Or (fmap (rewrite pairs) fs)
  Filter.Not f -> Filter.Not (rewrite pairs f)
  Filter.HasCardType _ -> predicate
  Filter.HasSupertype _ -> predicate
  Filter.HasColor _ -> predicate
  Filter.PowerAtLeast _ -> predicate
  Filter.ControlledBy _ -> predicate
  Filter.IsSource -> predicate
  Filter.IsPlayer _ -> predicate
  Filter.IsAttacking -> predicate
  Filter.AttackedThisTurn -> predicate
  Filter.IsAttachedToCreature -> predicate
  Filter.IsToken -> predicate
