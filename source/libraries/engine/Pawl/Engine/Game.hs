module Pawl.Engine.Game where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Card as Card
import qualified Pawl.Types.Asked as Asked
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.DamageEvent as DamageEvent
import qualified Pawl.Types.Discarded as Discarded
import Pawl.Types.Face (Face)
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LibraryPosition as LibraryPosition
import Pawl.Types.Mana (Mana)
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.Moved as Moved
import Pawl.Types.Object (Object)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

-- Ask a player a question and wait for the answer. The ONE way the engine
-- suspends: every prompt in the codebase goes through here, including the ones
-- that come by way of 'choose' below.
--
-- What it adds over lifting Program.prompt directly is the game the question
-- came from (#153). StateT sits outside the Program, so the interpreter cannot
-- see the state at a suspension; reading it here, on the inside, is what lets
-- Asked carry it. `enclosing` starts empty and Engine.playSubgame pushes onto it
-- from the frame above (CR 729.1a) -- this function cannot know it is inside a
-- subgame, and does not have to.
--
-- Here in the lowest engine layer because every prompting module imports it,
-- down to Pawl.Engine.Replacement, which must never import Pawl.Engine.Event.
ask :: Prompt.Prompt r -> Game r
ask p = do
  gs <- State.get
  Trans.lift (Program.prompt (Asked.MkAsked {Asked.enclosing = [], Asked.game = gs, Asked.prompt = p}))

-- CR 106.4: this player's mana pool. Absent from the map means an empty pool,
-- which is why every reader goes through here rather than through the Map. Here
-- rather than in Pawl.Engine.Mana because Pawl.Engine.ManaCount reads a pool
-- too and sits far below Mana; the convention needs one home both halves can
-- see.
poolOf :: PlayerId -> GameState -> Mana
poolOf pid gs = Map.findWithDefault (Mana.MkMana []) pid (GameState.manaPool gs)

lookupObject :: ObjectId -> GameState -> Maybe Object
lookupObject oid gs = Map.lookup oid (GameState.objects gs)

objectCount :: GameState -> Int
objectCount gs = Map.size (GameState.objects gs)

freshObjectId :: GameState -> (ObjectId, GameState)
freshObjectId gs =
  let ObjectId.MkObjectId n = GameState.nextObjectId gs
   in (ObjectId.MkObjectId n, gs {GameState.nextObjectId = ObjectId.MkObjectId (n + 1)})

freshTimestamp :: GameState -> (Timestamp.Timestamp, GameState)
freshTimestamp gs =
  let Timestamp.MkTimestamp n = GameState.nextTimestamp gs
   in (Timestamp.MkTimestamp n, gs {GameState.nextTimestamp = Timestamp.MkTimestamp (n + 1)})

-- Reject-not-repair, as payment already does: only a genuine permutation of the
-- offered indices is honoured. Anything else -- a short answer, a duplicate, an
-- out-of-range index -- leaves the canonical order standing rather than dropping
-- or duplicating an entry.
--
-- Beside 'choose' because it is what an ORDERING prompt's answer is applied
-- through, and both such prompts want it from modules that cannot see each
-- other: CR 603.3b's trigger order (Pawl.Engine.Engine) and CR 401.4's library
-- arrangement (Pawl.Engine.Resolve).
permute :: [a] -> [Natural] -> [a]
permute xs order =
  let canonical :: [Natural]
      canonical = zipWith const [0 ..] xs
      at i = case List.genericDrop i xs of
        h : _ -> Just h
        [] -> Nothing
   in if List.sort order == canonical
        then Maybe.mapMaybe at order
        else xs

-- Ask a player something they could have answered more than one way, and record
-- that they were asked (GameState.lastChoice). CR 104.4b's second sentence --
-- "loops that contain an optional action don't result in a draw" -- is why this
-- is a funnel rather than a note at each prompt site: being asked this is the
-- engine's whole definition of an optional action, and a new prompt site that
-- forgot to say so would make a loop containing it look mandatory.
--
-- FOUR prompts deliberately do not come through here, and are asked with a bare
-- 'ask' instead:
--
--   * Prompt.Concede, because CR 104.3a lets a player concede at any time, in a
--     loop or out of one. If conceding counted, no loop would ever be mandatory
--     and Pawl.Engine.Engine.checkMandatoryLoop could never fire.
--   * Prompt.ChooseAction when Pass is the only legal action -- passing is not a
--     decision. Engine.priorityLoop makes that call, being the only caller that
--     knows the menu.
--   * Prompt.Shuffle and Prompt.RandomFirstPlayer, which ask for RANDOMNESS
--     rather than for a choice (CR 701.24, CR 729.2). A loop that reshuffles a
--     library every cycle is still a loop of mandatory actions.
--
-- Every other prompt site already elides its prompt when the answer is forced,
-- so only the branch that genuinely asks reaches this.
choose :: Prompt.Prompt a -> Game a
choose p = do
  State.modify' (\gs -> gs {GameState.lastChoice = GameState.nextTimestamp gs})
  ask p

-- CR 400.2: a property of the ZONE and never of the card -- a hand every one of
-- whose cards is currently revealed is still a hidden zone.
--
-- Exhaustive rather than a membership test against a two-element list, so a new
-- Zone constructor cannot join the public side by default. It lives in this
-- lowest layer because Pawl.Engine.Activate (CR 602.2a's reveal) imports
-- Pawl.Engine.Event (CR 400.7e's proviso), so the only home both callers can
-- reach is one below them.
isHiddenZone :: Zone -> Bool
isHiddenZone zone = case zone of
  Zone.Library -> True
  Zone.Hand -> True
  Zone.Graveyard -> False
  Zone.Battlefield -> False
  Zone.Stack -> False
  Zone.Exile -> False
  -- CR 400.2 names the command zone among the public ones, alongside ante.
  Zone.Command -> False

zoneMembers :: Zone -> PlayerId -> GameState -> [ObjectId]
zoneMembers zone pid gs =
  let perPlayer m = foldMap (foldr (:) []) (Map.lookup pid m)
      ownedBy oid = case lookupObject oid gs of
        Just obj -> Object.owner obj == pid
        Nothing -> False
      ownedShared s = filter ownedBy (Set.toList s)
   in case zone of
        Zone.Library -> perPlayer (GameState.library gs)
        Zone.Hand -> perPlayer (GameState.hand gs)
        Zone.Graveyard -> perPlayer (GameState.graveyard gs)
        Zone.Battlefield -> ownedShared (GameState.battlefield gs)
        Zone.Exile -> ownedShared (GameState.exile gs)
        Zone.Command -> ownedShared (GameState.command gs)
        Zone.Stack -> filter ownedBy (GameState.stack gs)

-- CR 506.4: remove a permanent from combat. The one performer, shared by CR
-- 701.19a regeneration (Pawl.Engine.Replacement), an effect that specifically
-- removes it (Pawl.Engine.Resolve), and the two derived clauses -- a controller
-- change, and an attacking or blocking creature that stops being a creature
-- (both Pawl.Engine.Combat.removeChanged). It lives in this lowest layer because
-- Replacement needs it and must never import Pawl.Engine.Event.
--
-- `joinedUnder` loses its entry too: that map is CR 506.4's comparand for the
-- creatures currently in combat, so an object that has left must not stay in it.
--
-- The blockers map is edited two different ways on purpose, and the difference
-- is CR 509.1h's last sentence -- a creature remains blocked even once
-- everything blocking it has left combat:
--
--   * As an ATTACKER (the key), Map.delete drops the whole entry.
--   * As a BLOCKER (a member of some attacker's set), Set.delete removes only
--     the membership. `fmap` KEEPS the key, which is load-bearing: a surviving
--     key with an empty set is how "blocked, but nothing is currently blocking
--     it" is spelled, which Combat.isBlocked reads and CR 510.1c applies to.
--     Filtering the emptied entries away would turn the attacker unblocked and
--     let its damage through to the defending player.
removeFromCombat :: ObjectId -> GameState -> GameState
removeFromCombat oid gs =
  let c = GameState.combat gs
      c1 =
        c
          { Combat.attackers = Map.delete oid (Combat.attackers c),
            Combat.blockers = fmap (Set.delete oid) (Map.delete oid (Combat.blockers c)),
            Combat.joinedUnder = Map.delete oid (Combat.joinedUnder c)
          }
   in gs {GameState.combat = c1}

removeFromZones :: PlayerId -> ObjectId -> GameState -> GameState
removeFromZones pid oid gs =
  gs
    { GameState.library = Map.adjust (Seq.filter (/= oid)) pid (GameState.library gs),
      GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs),
      GameState.graveyard = Map.adjust (Seq.filter (/= oid)) pid (GameState.graveyard gs),
      GameState.battlefield = Set.delete oid (GameState.battlefield gs),
      -- CR 702.26k: "phased-out permanents owned by a player who leaves the game
      -- also leave the game", one of the three rules on the far side of CR 702.26b's
      -- "except for rules and effects that specifically mention phased-out
      -- permanents" -- so the battlefield line above does not reach them and this
      -- one has to. Rule 702.26k's second sentence ("this doesn't cause zone-change
      -- abilities to trigger") is free: nothing here funnels through
      -- Pawl.Engine.Event.
      GameState.phasedOut = Map.delete oid (GameState.phasedOut gs),
      GameState.exile = Set.delete oid (GameState.exile gs),
      GameState.command = Set.delete oid (GameState.command gs),
      GameState.stack = filter (/= oid) (GameState.stack gs)
    }

-- CR 401.2: a library is an ORDERED pile, so a library arrival needs the END it
-- arrives at. The Seq's HEAD is the top -- Event.drawCard takes the head, which
-- is CR 121.1's "top card of their library" -- so Top prepends and Bottom
-- appends.
--
-- Every other zone ignores the position, because none of them lets an effect
-- pick an end: the battlefield, exile and the command zone are Sets (CR 400.1's
-- shared zones, with no order at all), CR 402.3 lets a player arrange their hand
-- "in any convenient fashion", and the graveyard and the stack each have ONE
-- arrival rule of their own -- CR 404.1's "on top of its owner's graveyard" and
-- CR 405.2's "on top of all objects already there".
insertIntoZone :: Zone -> LibraryPosition.LibraryPosition -> PlayerId -> ObjectId -> GameState -> GameState
insertIntoZone zone position pid oid gs = case zone of
  Zone.Library ->
    let onto = case position of
          LibraryPosition.Top -> (Seq.><)
          LibraryPosition.Bottom -> flip (Seq.><)
     in gs {GameState.library = Map.insertWith onto pid (Seq.singleton oid) (GameState.library gs)}
  Zone.Hand -> gs {GameState.hand = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.hand gs)}
  Zone.Graveyard -> gs {GameState.graveyard = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.graveyard gs)}
  Zone.Battlefield -> gs {GameState.battlefield = Set.insert oid (GameState.battlefield gs)}
  Zone.Exile -> gs {GameState.exile = Set.insert oid (GameState.exile gs)}
  Zone.Command -> gs {GameState.command = Set.insert oid (GameState.command gs)}
  Zone.Stack -> gs {GameState.stack = oid : GameState.stack gs}

-- CR 608.2n: an ability leaves the stack and CEASES TO EXIST. No graveyard --
-- an ability is not a card -- so the removal is NOT a zone change and never
-- goes through Pawl.Engine.Event.changeZone: nothing arrives, so CR 400.7 mints
-- no new incarnation and CR 614 has no destination to replace.
--
-- The one way an ability object leaves the stack, and here rather than with the
-- resolution machinery because CR 608.2n's ending is not the only one that
-- needs it: CR 603.3c (Engine.placeBorne), CR 608.2a's failed intervening "if"
-- (Pawl.Engine.Stack), and CR 701.6a's countering (Pawl.Engine.Event.counter),
-- which cannot import Pawl.Engine.Resolve.
cease :: ObjectId -> GameState -> GameState
cease abilId gs =
  gs
    { GameState.stack = filter (/= abilId) (GameState.stack gs),
      GameState.objects = Map.delete abilId (GameState.objects gs)
    }

-- The card an object is a copy of. Nothing when the id is unknown.
cardOf :: ObjectId -> GameState -> Maybe Card
cardOf oid gs = cardOfSource (fmap Object.source (lookupObject oid gs))

-- CR 608.2h: `cardOf` for an object that may already be gone. The live object
-- first, then the record filed under the id it had while it existed.
--
-- Projection.viewWithLastKnown is the same fallback for an object's
-- CHARACTERISTICS; this is the one for its printed card, which the projection
-- cannot answer for -- CR 603.7's delayed-ability declarations are card data
-- and are never projected. Resolve's ArmDelayedTrigger calls it, its source
-- being able to exile itself an opcode earlier (Meandering Towershell); so does
-- its CreateCopy, whose named permanent can die in response to the trigger that
-- names it (Watchful Radstag).
--
-- A separate name rather than widening `cardOf`, whose many callers ask a
-- different question: "what card is this object" is about an object that IS
-- there, and answering it for one that is not would quietly resurrect a
-- permanent for every projection and quantity read that goes through it.
cardOfWithLastKnown :: ObjectId -> GameState -> Maybe Card
cardOfWithLastKnown oid gs = case lookupObject oid gs of
  Just obj -> cardOfSource (Just (Object.source obj))
  Nothing -> cardOfSource (fmap LastKnown.source (Map.lookup oid (GameState.lastKnown gs)))

-- The card behind a Source, if it has one. An ability on the stack does not: it
-- is an object in its own right (CR 113.7a), and the card is its SOURCE's.
cardOfSource :: Maybe Source.Source -> Maybe Card
cardOfSource mSource = case mSource of
  Nothing -> Nothing
  Just source -> case source of
    Source.OfCard printing -> Just (Printing.card printing)
    Source.OfToken card -> Just card
    Source.OfAbility _ _ -> Nothing
    Source.OfTrigger _ _ -> Nothing
    Source.OfEmblem card -> Just card
    Source.OfInherentTrigger _ _ -> Nothing

-- Narrow a card down to the face a (possibly absent) chosen name picks out --
-- CR 709.3b's half of resolveFaceFor below, which faceOf and faceOfWithLastKnown
-- both reach through, so the two cannot drift on what the fallback is.
--
-- Nothing (no face singled out) answers with the layout's own view (CR 709.4 /
-- 712.8a): a split card's characteristics are its two halves combined in every
-- zone but the stack. Just n (CR 709.3b) answers with only that named half's
-- characteristics -- looked up against the object's own STORED card, never a
-- projected one.
resolveFace :: Maybe CardName.CardName -> Card -> Face Card
resolveFace mName card = case mName of
  Nothing -> Card.combined card
  -- A name that does not resolve falls back to the combined view rather than
  -- failing. Pawl.CardSpec's "a card's face names are pairwise distinct" corpus
  -- lint holds that of every loadable card, which is what makes faceNamed's
  -- answer unique whenever the name IS one of the card's own faces, and all
  -- three writers of this field (Event.changeZoneAttaching's mkObj, which CR
  -- 709.3a's chosen half and CR 712.13's carried face both ride in on;
  -- Cast.asProposed, the gate's speculative
  -- stamp; and Resolve's CR 701.27a Transform arm) store a name they read from
  -- that same card's faces -- so this arm has no case that reaches it, short of
  -- a bug in one of them.
  Just n -> Maybe.fromMaybe (Card.combined card) (Card.faceNamed n card)

-- resolveFace for a reader that has the OBJECT and not just its chosen half --
-- the door CR 709.5's subtraction is behind.
--
-- A Room permanent's characteristics are neither of the two answers above. CR
-- 709.4's combined view is what the card has off the battlefield, and CR 709.3b's
-- single half is what a spell on the stack has; a Room on the battlefield has its
-- halves combined MINUS every locked one's name, mana cost and rules text
-- (Card.roomFace). Since it is not showing a half at all,
-- Event.changeZoneAttaching stores no `face` for it and its unlocked
-- designations (CR 709.5c) are the whole of what says which halves it has.
--
-- Gated on the BATTLEFIELD, which is CR 709.5c's own scope: the unlocked
-- designations are ones "a permanent on the battlefield can have", so a Room in a
-- hand, a graveyard or a library has none -- and reading no designations as no
-- unlocked halves there would give it no name and no text, when CR 709.4 gives it
-- both halves combined. On the STACK the ordinary reading stands: CR 709.3b
-- leaves only the cast half's characteristics, and CR 709.5b's exception to it is
-- about the halves' EXISTENCE being copiable rather than about which
-- characteristics the spell has.
resolveFaceFor :: Maybe Object.Object -> Card -> Face Card
resolveFaceFor mObj card = case mObj of
  Just obj
    | Card.hasSharedTypeLine card && Object.zone obj == Zone.Battlefield ->
        Card.roomFace (Object.unlockedHalves obj) card
  _ -> resolveFace (mObj >>= Object.face) card

-- CR 709.4a: the NAMES the object shows, the plural companion of resolveFaceFor
-- above -- one arm for each of that function's, in the same order, because the
-- two answer the same question about the same object and must not disagree
-- about which halves are showing.
--
-- Separate from resolveFaceFor rather than folded into it because a Face has one
-- name by construction: the combined view Face.name carries is the halves'
-- names joined for rendering (Pawl.Engine.Card.merge2), which is not a name the
-- object has. The set is what CR 709.4a's "one of its names" asks about.
namesFor :: Maybe Object.Object -> Card -> Set.Set CardName.CardName
namesFor mObj card = case mObj of
  Just obj
    | Card.hasSharedTypeLine card && Object.zone obj == Zone.Battlefield ->
        Card.roomNames (Object.unlockedHalves obj) card
  _ -> case mObj >>= Object.face of
    -- CR 709.4 / 712.8a / 715.4: the layout's own view, whose names are its
    -- contributing halves'.
    Nothing -> Card.combinedNames card
    -- CR 709.3b: a spell on the stack has only the named half's
    -- characteristics, so only that half's name -- with resolveFace's fallback
    -- to the combined view for a name that resolves to no face.
    Just n -> maybe (Card.combinedNames card) (Set.singleton . Face.name) (Card.faceNamed n card)

-- The face of the card an object is showing. Nothing when the id is unknown or
-- the object has no card behind it (an ability on the stack, CR 113.7a).
--
-- The seam every characteristic read goes through -- which is why CR 708.2's
-- substitution is HERE and nowhere else. "Face-down spells and face-down
-- permanents have no characteristics other than those listed by the ability or
-- rules that allowed the spell or permanent to be face down", and CR 708.2a
-- fixes the list for an ability that names none. Those listed values are the
-- object's COPIABLE values, so they replace the printed face at the point every
-- read starts from rather than being folded in as a CR 613 layer -- see
-- Card.faceDownFace.
--
-- The object still HAS its card (Object.source), and faceUpFaceOf below is how
-- CR 702.37e reads the morph cost off it. Nothing hides the card from a reader
-- that goes looking, so CR 708.5's "you can't look at face-down permanents
-- controlled by another player" is unimplemented (#682).
faceOf :: ObjectId -> GameState -> Maybe (Face Card)
faceOf oid gs = case fmap Object.facing (lookupObject oid gs) of
  Just Facing.FaceDown -> Just Card.faceDownFace
  _ -> faceUpFaceOf oid gs

-- CR 201.1 / 709.4a: the names of the object an id names -- `faceOf`'s plural
-- companion, and the value Pawl.Engine.Projection.baseCharacteristics seeds
-- ProjectedCharacteristics.names from.
--
-- EMPTY for a face-down object, which is CR 708.2a's "no name" said in the one
-- way a set can say it, and empty again for an id that names nothing or an
-- object with no card behind it (CR 113.7a: an ability on the stack).
namesOf :: ObjectId -> GameState -> Set.Set CardName.CardName
namesOf oid gs = case fmap Object.facing (lookupObject oid gs) of
  Just Facing.FaceDown -> Set.empty
  _ -> maybe Set.empty (namesFor (lookupObject oid gs)) (cardOf oid gs)

-- `faceOf` IGNORING CR 708.2's substitution: what the object's own card shows,
-- whichever way up the object is -- CR 709.3b's chosen half, CR 709.4's combined
-- view or CR 709.5's subtracted one, whichever resolveFaceFor answers with, but
-- never Card.faceDownFace.
--
-- Two rules want it, and both are about a face-down object without being about
-- its characteristics. CR 702.37e reads "what the permanent's morph cost WOULD
-- BE if it were face up" -- a face-down permanent has no keywords for a
-- projected read to find, so the cost has to come from the card. CR 708.8's
-- turning face up then reverts the copiable values to these.
--
-- Deliberately NOT the door any characteristic read may take: a reader that
-- wants to know what an object IS wants faceOf.
faceUpFaceOf :: ObjectId -> GameState -> Maybe (Face Card)
faceUpFaceOf oid gs = do
  card <- cardOf oid gs
  Just (resolveFaceFor (lookupObject oid gs) card)

-- `faceOf`, narrowed to the one characteristic that is not always read off the
-- live face: CR 712.8e calculates a nonmodal double-faced permanent's mana value
-- from its FRONT face's mana cost even while its back face is up. Every mana
-- value read goes through here rather than through faceOf, and
-- Pawl.Engine.Card.manaCostFace is where the layout decides.
--
-- Its own function rather than a flag on faceOf, because the two answers differ
-- for exactly one object -- a transformed permanent -- and every OTHER
-- characteristic of that permanent is its back face's (CR 712.8e's first
-- sentence). Collapsing them either way is a silent wrong answer.
--
-- CR 708.2a's "no mana cost" wins over CR 712.8e, and the rules leave no room
-- for it not to: a face-down permanent's characteristics are the listed ones and
-- nothing else, so there is no front face for that rule to reach back to. The
-- mana value that falls out is CR 202.3a's 0.
manaCostFaceOf :: ObjectId -> GameState -> Maybe (Face Card)
manaCostFaceOf oid gs = case fmap Object.facing (lookupObject oid gs) of
  Just Facing.FaceDown -> Just Card.faceDownFace
  _ -> do
    card <- cardOf oid gs
    Just (Card.manaCostFace card (resolveFaceFor (lookupObject oid gs) card))

-- `faceOf` for an object that may already be gone -- cardOfWithLastKnown's
-- fallback, for its reasons (CR 608.2h). Shares resolveFace with faceOf: if the
-- object is still live, its own `face` narrows the answer the same way faceOf's
-- does; if it is gone, `lookupObject` answers Nothing and this falls back to
-- the combined view, because LastKnown carries no face of its own for a later
-- read to recover (#654).
--
-- CR 708.2's substitution applies while the object is still live, for faceOf's
-- reason. Once it is gone there is nothing to apply it from -- LastKnown carries
-- no facing any more than it carries a face (#654) -- and CR 708.9 is what makes
-- that the right answer anyway: a face-down permanent leaving the battlefield is
-- revealed to all players as it goes.
faceOfWithLastKnown :: ObjectId -> GameState -> Maybe (Face Card)
faceOfWithLastKnown oid gs = case fmap Object.facing (lookupObject oid gs) of
  Just Facing.FaceDown -> Just Card.faceDownFace
  _ -> do
    card <- cardOfWithLastKnown oid gs
    Just (resolveFaceFor (lookupObject oid gs) card)

-- CR 701.27a over ONE object: "turn it over so that its other face is up", or
-- leave the map exactly as it was. The primitive BOTH transform paths share --
-- Pawl.Engine.Resolve's CR 701.27a opcode, which adds CR 701.27f's already-turned
-- gate on top, and Pawl.Engine.Daytime's CR 702.145c/f sweep, which has no such
-- gate because that rule turns a permanent over from a STATIC ability rather than
-- from an ability on the stack.
--
-- Three ways nothing happens, and none is an error:
--
--   * the id names nothing on the BATTLEFIELD. CR 701.27a transforms a
--     PERMANENT, which CR 110.1 makes an object on the battlefield.
--   * the id names nothing at all, or nothing with a card behind it (CR 113.7a).
--   * Card.turnedOver declines -- CR 701.27c's card that is not double-faced,
--     CR 701.27d's instant or sorcery face.
--
-- ONE FIELD, in place, because CR 712.18 says the permanent is not a new object:
-- "when a double-faced permanent transforms or converts, it doesn't become a new
-- object. Any effects that applied to that permanent will continue to apply to
-- it." So no id is minted, no timestamp is reissued, and damage, counters and
-- attachments ride through untouched. CR 400.7 is the negative half of the same
-- claim: this is not a zone change, so nothing mints an incarnation.
--
-- Reads the object's OWN card (cardOf), never a projected one, which is the
-- footing Object.face is stored on: CR 712.9's first Example turns on a Clone
-- being a one-faced card whatever it copied, and that is the same read.
--
-- `now` is supplied by the caller rather than minted here, because a caller that
-- turns SEVERAL permanents over does so simultaneously -- CR 608.2f for one
-- Moonmist, CR 702.145c for one nightfall -- and a later CR 701.27f comparison
-- must not be able to tell them apart.
turnFaceOver :: Timestamp.Timestamp -> GameState -> ObjectId -> Map.Map ObjectId Object -> Map.Map ObjectId Object
turnFaceOver now gs oid objects
  | not (Set.member oid (GameState.battlefield gs)) = objects
  | otherwise = case (Map.lookup oid objects, cardOf oid gs) of
      (Just object, Just card) -> case Card.turnedOver (Object.face object) card of
        Nothing -> objects
        Just name -> Map.insert oid object {Object.face = Just name, Object.turnedOverAt = Just now} objects
      _ -> objects

-- CR 712.11: is this object showing the FRONT face of its card? The question CR
-- 702.145c asks of a daybound permanent and CR 702.145f asks (inverted) of a
-- nightbound one.
--
-- Nothing in Object.face IS the front face, not an unknown one: CR 712.11 casts a
-- double-faced spell with its front face up by default, so a permanent nothing
-- has singled a face out for is showing its front, and resolveFace above answers
-- with the layout's own view for the same reason. Card.faces is a NonEmpty, so
-- the head is total and a card with no front face cannot arise.
--
-- An object with no card behind it (CR 113.7a: an ability on the stack) answers
-- True as well, which is the same answer as "nothing has turned it over" and is
-- what its only caller wants: Pawl.Engine.Daytime asks this of battlefield
-- permanents, where CR 110.1 leaves no such object, and reads it beside a
-- keyword test that such an object fails anyway.
isFrontFaceUp :: ObjectId -> GameState -> Bool
isFrontFaceUp oid gs = case lookupObject oid gs >>= Object.face of
  Nothing -> True
  Just name -> case cardOf oid gs of
    Nothing -> True
    Just card -> Face.name (NonEmpty.head (Card.Type.faces card)) == name

-- CR 112.1: a spell is a card on the stack. Asks the object's zone AND its KIND
-- (its Source) -- a classification, never the card's identity.
isSpell :: ObjectId -> GameState -> Bool
isSpell oid gs = case lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Object.zone obj == Zone.Stack && case Object.source obj of
      Source.OfCard _ -> True
      Source.OfToken _ -> False
      Source.OfAbility _ _ -> False
      Source.OfTrigger _ _ -> False
      Source.OfEmblem _ -> False
      Source.OfInherentTrigger _ _ -> False

-- CR 113.9: is this object an activated or triggered ability on the stack? The
-- SIBLING of isSpell above and never its complement, asking the same two
-- things: the object's zone and its KIND, never the card's identity.
--
-- The two together are what countering needs: an effect that counters only
-- spells must not reach an ability, and one that counters abilities must not
-- reach a spell. Pawl.Engine.Event.counter branches on this to choose between
-- CR 701.6a's graveyard and CR 608.2n's cease.
--
-- CR 725.2's sourceless inherent monarch triggers are triggered abilities all
-- the same, so a Stifle may counter one. An emblem stays in the command zone
-- (CR 114.1 / 114.4) and a token on the stack would be a spell copy (CR
-- 112.1a), so both answer False whatever zone they are found in.
isAbility :: ObjectId -> GameState -> Bool
isAbility oid gs = case lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Object.zone obj == Zone.Stack && case Object.source obj of
      Source.OfCard _ -> False
      Source.OfToken _ -> False
      Source.OfAbility _ _ -> True
      Source.OfTrigger _ _ -> True
      Source.OfEmblem _ -> False
      Source.OfInherentTrigger _ _ -> True

-- CR 110.5: a permanent's tapped/untapped status. CR 110.5d gives status only
-- to a permanent, so an unknown id -- and a card outside the battlefield -- is
-- reported untapped, which is the answer every other status arm gives for a
-- nonexistent object too.
isTapped :: ObjectId -> GameState -> Bool
isTapped oid gs = case lookupObject oid gs of
  Nothing -> False
  Just obj -> Object.tapped obj == TapState.Tapped

-- CR 111.1 / 111.6: is this object a token rather than a card? Asks the
-- object's KIND, a classification in the same standing as isSpell, never the
-- card's identity. False for an unknown id and for every non-token kind.
isToken :: ObjectId -> GameState -> Bool
isToken oid gs = maybe False (sourceIsToken . Object.source) (lookupObject oid gs)

-- The same question of a bare Source, which is what CR 608.2h's record keeps of
-- an object that has ceased (LastKnown.source). One classification, so a live
-- object and a departed one cannot answer it differently.
sourceIsToken :: Source.Source -> Bool
sourceIsToken source = case source of
  Source.OfToken _ -> True
  Source.OfCard _ -> False
  Source.OfAbility _ _ -> False
  Source.OfTrigger _ _ -> False
  Source.OfEmblem _ -> False
  Source.OfInherentTrigger _ _ -> False

-- CR 104.2a: who is still in the game. Here rather than in
-- Pawl.Engine.Departure because Departure imports Pawl.Engine.Monarch, which
-- imports Pawl.Engine.Event, so the event pipeline (Event.createTokens, for CR
-- 800.4b) could not ask the question through Departure without an import cycle.
stillPlaying :: GameState -> [PlayerId]
stillPlaying gs =
  let isPlaying entry = Player.status (snd entry) == Status.Playing
   in fmap fst (filter isPlaying (Map.toList (GameState.players gs)))

-- Who is still in the game, in SEATING order. stillPlaying reads the players
-- map, so it comes back in PlayerId order; GameState.turnOrder is the permanent
-- seating roster (CR 800.5, CR 806.3). The order is load-bearing: CR 103.5 has
-- the starting player declare their mulligan first, then each other player in
-- turn order, and CR 727.1a / CR 729.2 rotate the rebuilt order to begin with
-- them.
stillPlayingInOrder :: GameState -> [PlayerId]
stillPlayingInOrder gs =
  let playing = stillPlaying gs
   in filter (\pid -> List.elem pid playing) (GameState.turnOrder gs)

-- CR 101.4: the seating roster rotated to APNAP order. The shared anchor for CR
-- 603.3b (Engine.apnapPlayers, stacking triggers) and CR 121.2c
-- (Pawl.Engine.Resolve's Draw arm).
--
-- SEATING, not survival: turnOrder is the permanent roster (CR 800.5), so a
-- departed seat is still named here, and a caller that must not name one
-- filters with stillPlaying. An active player somehow absent from the roster
-- degrades to the roster itself rather than to nobody.
apnapOrder :: GameState -> [PlayerId]
apnapOrder gs =
  let order = GameState.turnOrder gs
      active = GameState.activePlayer gs
   in dropWhile (/= active) order <> takeWhile (/= active) order

-- CR 701.24a: shuffling randomises an ORDER, so it is a permutation -- the
-- cards that were there are the cards that are there.
--
-- FILTERED, NOT TRUSTED, the posture Combat.declareAttackers and
-- Engine.priorityLoop take toward their own answers (#219). This one matters
-- more than most: a shuffle answer BECOMES the zone, so an unchecked answer can
-- duplicate a card, drop one, or name an id that was never in the library --
-- inventing or destroying objects rather than merely breaking a rule (#222).
--
-- Sorted comparison, so a genuine reordering is honoured and only a change of
-- CONTENTS is refused. Rejecting keeps the existing order: reject, not repair.
honourShuffle :: [ObjectId] -> [ObjectId] -> [ObjectId]
honourShuffle offered answer =
  if List.sort answer == List.sort offered
    then answer
    else offered

-- The caster an event describes, if it is a cast (CR 601.2i).
--
-- HERE rather than beside Pawl.Engine.Event's other GameEvent classifiers, and
-- only because of the import graph: its one caller is
-- Pawl.Engine.PlayerEffect.castsThisTurn, and Pawl.Engine.Event now asks that
-- module CR 701.6a's counterability question -- so an Event.castOf would put
-- the two modules in a cycle. Nothing about GameEvent is owned by Event --
-- Pawl.Engine.Projection and Pawl.Engine.Count already case on it too.
castOf :: GameEvent -> Maybe PlayerId
castOf event = case event of
  GameEvent.SpellCast (SpellWasCast.MkSpellWasCast pid _ _) -> Just pid
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BlockerDeclared {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing

-- The discarding player an event describes, if it is a discard (CR 701.9a).
--
-- castOf's sibling, and here for its import-graph reason: the caller is
-- Pawl.Engine.Quantity's CardsDiscardedThisTurn arm, and Pawl.Engine.Event
-- imports that module's callers rather than the other way about.
--
-- The Moved event the same discard files is deliberately not an arm, and the zone
-- change is the wrong record in both directions: CR 701.1 and CR 701.9a make
-- discarding the keyword ACTION of moving a card from a hand to a graveyard, so a
-- move no effect performed as a discard is not one, and CR 701.9c leaves a
-- redirected discard still discarded though it never reached a graveyard.
--
-- The CAUSE is not consulted, CR 702.29a making a cycled card a discarded one.
discardOf :: GameEvent -> Maybe PlayerId
discardOf event = case event of
  GameEvent.Discarded (Discarded.MkDiscarded pid _ _) -> Just pid
  -- CR 701.9a's discard moves a card OUT of a hand; CR 121.1's draw moves one
  -- in. Opposite directions, and neither event stands in for the other.
  GameEvent.Drew {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BlockerDeclared {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing

-- The permanent an event describes ENTERING THE BATTLEFIELD, if it is one. CR
-- 400.7's zone change with a destination of Zone.Battlefield, which is what the
-- glossary's "enters the battlefield" is and what a bare "enters" on a card
-- abbreviates.
--
-- The id is ZoneChange.object and not ZoneChange.departed: CR 400.7 makes the
-- entrant a NEW object, and the caller asks about the permanent that is on the
-- battlefield now. The same choice Pawl.Engine.Event's PermanentEnters arm makes
-- for CR 603.6a, so a trigger and this reader agree on what entered.
--
-- castOf's and discardOf's sibling, and here for their import-graph reason: the
-- caller is Pawl.Engine.Quantity's EnteredThisTurn arm.
enteredBattlefield :: GameEvent -> Maybe ObjectId
enteredBattlefield event = case event of
  GameEvent.Moved (Moved.MkMoved change _) ->
    if ZoneChange.to change == Zone.Battlefield then Just (ZoneChange.object change) else Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BlockerDeclared {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing

-- The PLAYER an event describes being dealt damage, if it describes one. CR
-- 120.1's damage with a Recipient.ToPlayer, which is the only recipient CR 120.3a
-- calls a player: damage to a permanent a player controls -- their creature, their
-- planeswalker, a battle they protect -- is not damage to them, and CR 120.3c and
-- CR 120.3h are what it is instead.
--
-- castOf's, discardOf's and enteredBattlefield's sibling, and here for their
-- import-graph reason: the caller is Pawl.Engine.Quantity's
-- PlayersDealtDamageThisTurn arm.
--
-- The `amount > 0` test is CR 120.8 -- "if a source would deal 0 damage, it does
-- not deal damage at all" -- and a FENCE rather than a reachable filter on the
-- pool as it stands: Resolve's DealDamage gates on `n > 0`, combat events are
-- built with amount > 0 (CR 510.1a), and a fully prevented event is dropped
-- rather than shrunk to nothing (CR 615.6, Event.applyDamageRewrite's PreventNext
-- arm). What could still reach here is a DamageRewrite.SetAmount 0 or a
-- Scale to 0, and neither has a producer in the pool.
--
-- The LifeLost event the same damage also files is deliberately not an arm. CR
-- 120.3a makes the life loss a RESULT of the damage rather than the damage, so
-- reading it would count an effect's bare "loses 2 life" as damage; and CR 120.3b's
-- infect damage causes no life loss at all, yet it is damage dealt to that player.
damagedPlayer :: GameEvent -> Maybe PlayerId
damagedPlayer event = case event of
  GameEvent.DamageDealt ev ->
    if DamageEvent.amount ev > 0
      then case DamageEvent.target ev of
        Recipient.ToPlayer pid -> Just pid
        Recipient.ToCreature _ -> Nothing
        Recipient.ToPlaneswalker _ -> Nothing
        Recipient.ToBattle _ -> Nothing
        Recipient.ToObject _ -> Nothing
      else Nothing
  -- CR 615.13's record of damage that did NOT happen, which is the opposite fact.
  GameEvent.DamagePrevented {} -> Nothing
  -- CR 120.3a's result, not the damage; see the haddock above.
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.Moved {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BlockerDeclared {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  GameEvent.SpellCountered _ -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
