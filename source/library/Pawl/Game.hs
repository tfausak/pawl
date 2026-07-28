module Pawl.Game where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Pawl.Type.Card (Card)
import qualified Pawl.Type.Combat as Combat
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.Object (Object)
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Timestamp as Timestamp
import Pawl.Type.Zone (Zone)
import qualified Pawl.Type.Zone as Zone

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

-- CR 506.4: remove a permanent from combat -- it "stops being an attacking,
-- blocking, blocked, and/or unblocked creature". The one performer, shared by the
-- clauses that have producers: CR 701.19a regeneration (Pawl.Replacement) and a
-- controller change (Pawl.Combat.removeControlChanged).
-- Edits the GameState.combat maps directly. It lives here, in the lowest layer,
-- because Pawl.Replacement needs it and must never import Pawl.Event.
--
-- Combat.joinedUnder loses its entry too: the map is the CR 506.4 comparand for
-- creatures IN combat, and this is what takes one out.
--
-- The blockers map is edited two different ways on purpose, and the difference is
-- CR 509.1h's last sentence: "A creature remains blocked even if all the creatures
-- blocking it are removed from combat."
--
--   * As an ATTACKER (the key), oid stops being attacking and blocked outright --
--     CR 506.4's "stops being an attacking, blocking, blocked, and/or unblocked
--     creature" -- so Map.delete drops the whole entry.
--   * As a BLOCKER (a member of some attacker's set), Set.delete removes only the
--     membership. `fmap` KEEPS the key, and that is load-bearing rather than
--     incidental: an attacker's key surviving with an empty set is exactly how
--     "blocked, but nothing is currently blocking it" is spelled, which is what
--     Combat.isBlocked reads and what CR 510.1c's "if no creatures are currently
--     blocking it ... it assigns no combat damage" then applies to. Map.filter-ing
--     the emptied entries away would silently turn the attacker unblocked and let
--     its damage through to the defending player.
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
      GameState.exile = Set.delete oid (GameState.exile gs),
      GameState.command = Set.delete oid (GameState.command gs),
      GameState.stack = filter (/= oid) (GameState.stack gs)
    }

insertIntoZone :: Zone -> PlayerId -> ObjectId -> GameState -> GameState
insertIntoZone zone pid oid gs = case zone of
  Zone.Library -> gs {GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs)}
  Zone.Hand -> gs {GameState.hand = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.hand gs)}
  Zone.Graveyard -> gs {GameState.graveyard = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.graveyard gs)}
  Zone.Battlefield -> gs {GameState.battlefield = Set.insert oid (GameState.battlefield gs)}
  Zone.Exile -> gs {GameState.exile = Set.insert oid (GameState.exile gs)}
  Zone.Command -> gs {GameState.command = Set.insert oid (GameState.command gs)}
  Zone.Stack -> gs {GameState.stack = oid : GameState.stack gs}

-- The card an object is a copy of. Nothing when the id is unknown.
cardOf :: ObjectId -> GameState -> Maybe Card
cardOf oid gs = case lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.source obj of
    Source.OfCard printing -> Just (Printing.card printing)
    Source.OfToken card -> Just card
    Source.OfAbility _ _ -> Nothing
    Source.OfTrigger _ _ -> Nothing
    Source.OfEmblem card -> Just card
    Source.OfInherentTrigger _ _ -> Nothing

-- CR 112.1: a spell is a card on the stack. This asks the object's zone AND its
-- KIND (its Source) -- a classification like isPermanent (Stack.resolveTop), never
-- the card's identity. Only a card (OfCard) currently on the stack is a spell: a
-- token is never a spell, and a card off the stack (hand, a battlefield permanent,
-- graveyard) is not one either.
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

-- CR 104.2a: who is still in the game. A pure query over the players map, so it
-- lives here with the other GameState accessors rather than in Pawl.Departure
-- with the machinery that acts on leaving. That is not only tidiness: Departure
-- imports Pawl.Monarch (CR 725.4 reassignment happens inside `depart`), and
-- Monarch imports Pawl.Event, so anything in the event pipeline that needs to
-- ask "is this player still in the game" -- Event.createTokens does, for CR
-- 800.4b -- cannot reach it through Departure without an import cycle.
stillPlaying :: GameState -> [PlayerId]
stillPlaying gs =
  let isPlaying entry = Player.status (snd entry) == Status.Playing
   in fmap fst (filter isPlaying (Map.toList (GameState.players gs)))

-- Who is still in the game, in SEATING order.
--
-- stillPlaying reads the players map, so it comes back in PlayerId order.
-- GameState.turnOrder is the permanent seating roster (CR 800.5, CR 806.3; see
-- Pawl.Type.GameState), so anything that REBUILDS a turn order or walks seats
-- needs this instead. The order is load-bearing, not cosmetic: CR 103.5 has the
-- starting player declare their mulligan first, then each other player in turn
-- order, and CR 727.1a / CR 729.2 rotate the rebuilt order to begin with the
-- starting player.
stillPlayingInOrder :: GameState -> [PlayerId]
stillPlayingInOrder gs =
  let playing = stillPlaying gs
   in filter (\pid -> List.elem pid playing) (GameState.turnOrder gs)

-- CR 101.4: the seating roster rotated to APNAP order -- the active player
-- first, then each other player in turn order, wrapping at the end of the
-- roster. The shared anchor for every "the active player goes first, then each
-- other player in turn order" rule: CR 603.3b, which Engine.apnapPlayers reads
-- to stack triggers, and CR 121.2c, which Pawl.Resolve's Draw arm reads to
-- order drawers.
--
-- SEATING, not survival: turnOrder is the permanent roster (CR 800.5), so a
-- departed seat is still named here. A caller that must not name one filters
-- with stillPlaying, the way apnapPlayers does. An active player somehow absent
-- from the roster degrades to the roster itself rather than to nobody.
apnapOrder :: GameState -> [PlayerId]
apnapOrder gs =
  let order = GameState.turnOrder gs
      active = GameState.activePlayer gs
   in dropWhile (/= active) order <> takeWhile (/= active) order

-- CR 701.24a: "To shuffle a library or a face-down pile of cards, randomize the
-- cards within it so that no player knows their order." Randomising an ORDER is a
-- permutation -- the cards that were there are the cards that are there.
--
-- FILTERED, NOT TRUSTED, the posture Combat.declareAttackers and
-- Engine.priorityLoop take toward their own answers (#219). This one matters more
-- than most: a shuffle answer BECOMES the zone, so an unchecked answer can
-- duplicate a card, drop one, or name an id that was never in the library --
-- inventing or destroying objects outright rather than merely breaking a rule
-- (#222).
--
-- Sorted comparison, so a genuine reordering is honoured and only a change of
-- CONTENTS is refused. Rejecting keeps the existing order, which is the
-- reject-not-repair posture rather than an attempt to guess what was meant.
honourShuffle :: [ObjectId] -> [ObjectId] -> [ObjectId]
honourShuffle offered answer =
  if List.sort answer == List.sort offered
    then answer
    else offered
