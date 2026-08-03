module Pawl.Engine.Game where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Pawl.Types.Card (Card)
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LastKnown as LastKnown
import Pawl.Types.Mana (Mana)
import qualified Pawl.Types.Mana as Mana
import Pawl.Types.Object (Object)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Timestamp as Timestamp
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone

-- CR 106.4: this player's mana pool. Absent from the map means an empty pool,
-- which is why every reader goes through here rather than through the Map.
--
-- Here rather than in Pawl.Engine.Mana because Pawl.Engine.ManaCount reads a
-- pool too (Omnath, Locus of Mana), and Pawl.Engine.Mana sits far above it: Mana
-- imports Pawl.Engine.Projection, which imports Pawl.Engine.Quantity, which is
-- what ties the ManaCount arm. The "absent means empty" convention must have one
-- home, so the accessor moved to the module both halves can see.
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

-- CR 400.2: "Public zones are zones in which all players can see the cards'
-- faces ... Library and hand are hidden zones, even if all the cards in one such
-- zone happen to be revealed." That trailing clause is why this is a property of
-- the ZONE and never of the card: a hand every one of whose cards is currently
-- revealed is still a hidden zone.
--
-- Exhaustive rather than a membership test against a two-element list, so a new
-- Zone constructor cannot join the public side by default.
--
-- It lives HERE, in the lowest layer, rather than beside either of its two
-- callers: Pawl.Engine.Activate reads it for CR 602.2a's reveal and
-- Pawl.Engine.Event for CR 400.7e's public-zone proviso, and Activate imports
-- Event, so the only home both can reach is one below them.
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

-- CR 506.4: remove a permanent from combat -- it "stops being an attacking,
-- blocking, blocked, and/or unblocked creature". The one performer, shared by the
-- clauses that have producers: CR 701.19a regeneration (Pawl.Engine.Replacement), an
-- effect that specifically removes it (Pawl.Engine.Resolve), and the two derived
-- clauses -- a controller change, and an attacking or blocking creature that
-- stops being a creature (both Pawl.Engine.Combat.removeChanged).
-- Edits the GameState.combat maps directly. It lives here, in the lowest layer,
-- because Pawl.Engine.Replacement needs it and must never import Pawl.Engine.Event.
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

-- CR 608.2n: an ability leaves the stack and CEASES TO EXIST -- "as the final
-- part of an ability's resolution, the ability is removed from the stack and
-- ceases to exist". No graveyard: an ability is not a card, so there is no
-- owner's graveyard for it to go to, and the removal is therefore NOT a zone
-- change and never goes through Pawl.Engine.Event.changeZone (nothing arrives,
-- so CR 400.7 mints no new incarnation and CR 614 has no destination to
-- replace).
--
-- The one way an ability object leaves the stack, and it lives HERE rather than
-- with the resolution machinery because rule 608.2n's ending is not the only one
-- that needs it. CR 603.3c: "if no mode is chosen, the ability is removed from
-- the stack" (Pawl.Engine.Engine.placeBorne). CR 608.2a, an intervening "if"
-- that is no longer true: "the ability is removed from the stack and does
-- nothing" (Pawl.Engine.Stack). And CR 701.6a's countering of an ability ends
-- the same way (Pawl.Engine.Event.counter) -- which is what moved this out of
-- Pawl.Engine.Resolve, since Event cannot import that module.
cease :: ObjectId -> GameState -> GameState
cease abilId gs =
  gs
    { GameState.stack = filter (/= abilId) (GameState.stack gs),
      GameState.objects = Map.delete abilId (GameState.objects gs)
    }

-- The card an object is a copy of. Nothing when the id is unknown.
cardOf :: ObjectId -> GameState -> Maybe Card
cardOf oid gs = cardOfSource (fmap Object.source (lookupObject oid gs))

-- CR 608.2h: `cardOf` for an object that may already be gone -- "if the effect
-- requires information from a specific object, INCLUDING THE SOURCE OF THE
-- ABILITY ITSELF, the effect uses the current information of that object if it's
-- in the public zone it was expected to be in; if it's no longer in that zone ...
-- the effect uses the object's last known information." The live object first,
-- then the record filed under the id it had while it existed.
--
-- Projection.viewWithLastKnown is the same fallback for an object's
-- CHARACTERISTICS; this is the one for its printed card, which the projection
-- cannot answer for -- CR 603.7's delayed-ability declarations are card data and
-- are never projected (see Pawl.Types.Card.delayedAbilities). Its one caller is
-- Resolve's ArmDelayedTrigger, whose source can have exiled itself an opcode
-- earlier (Meandering Towershell).
--
-- A separate name rather than widening `cardOf`, whose ~20 callers ask a
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

-- CR 113.9: is this object an activated or triggered ability on the stack?
-- "Activated and triggered abilities on the stack aren't spells" -- so this is
-- the SIBLING of isSpell just above and never its complement, and it asks the
-- same two things in the same way: the object's zone, and its KIND (its Source),
-- never the card's identity.
--
-- The two together are what CR 113.9's rule about countering needs: an effect
-- that counters only spells must not reach an ability, and one that specifically
-- counters abilities must not reach a spell. Pawl.Types.Pool.Spells and
-- Pawl.Types.Pool.Abilities are those two answers as target pools, and
-- Pawl.Engine.Event.counter branches on this one to choose between rule 701.6a's
-- graveyard and CR 608.2n's cease.
--
-- CR 725.2's inherent monarch triggers count: "there are two inherent triggered
-- abilities associated with being the monarch. These triggered abilities have no
-- source", and they are triggered abilities all the same, so a Stifle may
-- counter one. An emblem goes into the command zone and stays there (CR 114.1 /
-- 114.4), and a token on the stack would be a spell copy (CR 112.1a) rather than
-- an ability, so both answer False on their own, whatever zone they are found
-- in.
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

-- CR 111.1 / 111.6: is this object a token rather than a card? Asks the object's
-- KIND (its Source), a classification in the same standing as isSpell just above,
-- never the card's identity. False for an unknown id, and for every non-token
-- kind -- an ability or trigger on the stack has no permanent behind it at all,
-- and an emblem (CR 114.5) is not a permanent either.
isToken :: ObjectId -> GameState -> Bool
isToken oid gs = case lookupObject oid gs of
  Nothing -> False
  Just obj -> case Object.source obj of
    Source.OfToken _ -> True
    Source.OfCard _ -> False
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
    Source.OfEmblem _ -> False
    Source.OfInherentTrigger _ _ -> False

-- CR 104.2a: who is still in the game. A pure query over the players map, so it
-- lives here with the other GameState accessors rather than in Pawl.Engine.Departure
-- with the machinery that acts on leaving. That is not only tidiness: Departure
-- imports Pawl.Engine.Monarch (CR 725.4 reassignment happens inside `depart`), and
-- Monarch imports Pawl.Engine.Event, so anything in the event pipeline that needs to
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
-- Pawl.Types.GameState), so anything that REBUILDS a turn order or walks seats
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
-- to stack triggers, and CR 121.2c, which Pawl.Engine.Resolve's Draw arm reads to
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
