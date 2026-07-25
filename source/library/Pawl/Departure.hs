module Pawl.Departure where

import Control.Applicative ((<|>))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Game as Game
import qualified Pawl.Monarch as Monarch
import qualified Pawl.Projection as Projection
import qualified Pawl.Type.Combat as Combat
import qualified Pawl.Type.Decider as Decider
import Pawl.Type.Departure (Departure)
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Player as Player
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Result (Result)
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Zone as Zone

-- CR 104.2a / 104.3: who is still in the game, and what happens when someone
-- leaves it.
--
-- Split out of Pawl.Sba because leaving is not always a state-based action. CR
-- 104.3b (life <= 0) is one and arrives through the SBA pass; CR 104.3a (concede)
-- is IMMEDIATE and Pawl.Engine reaches it directly. Having Engine call into
-- Pawl.Sba for something the rules say is not a state-based action would misstate
-- the rules in the module graph.
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

-- Mark a player as having left, with the reason they left, and perform
-- everything the rules attach to that moment. Pure, because the CR 704.5 pass
-- folds it over several players before recomputing the outcome once.
--
-- CR 725.4 belongs INSIDE this function, not after it: "the active player becomes
-- the monarch at the same time as that player leaves the game." Both doors --
-- Departure.leaveGame (CR 104.3a) and Pawl.Sba's fold (CR 704.5) -- get it by
-- construction rather than by remembering to call it.
depart :: Departure -> PlayerId -> GameState -> GameState
depart reason pid gs =
  let lose p = p {Player.status = Status.Departed reason}
      -- CR 800.4a's own ordering ("Then ... Then ...") is load-bearing, so the
      -- clauses are composed in the rule's order and nothing here reorders them.
      -- They run before the status flip, and THAT much is arbitrary: no clause
      -- reads Player.status (Projection.controllerOf is an object's owner
      -- overridden by a layer-2 SetController and nothing else), and
      -- continuesAfterDeparture reads GameState.turnOrder, which the flip does
      -- not touch. The flip's position is load-bearing only for the monarch
      -- call below, which Monarch.reassignOnDeparture requires to have already
      -- happened -- see its own haddock.
      settled =
        if continuesAfterDeparture gs
          then remainingControlledExiled pid (nonCardStackObjectsCease pid (controlEffectsEnd pid (objectsLeaveWith pid gs)))
          else gs
      flipped = settled {GameState.players = Map.adjust lose pid (GameState.players settled)}
   in Monarch.reassignOnDeparture pid (stillPlayingInOrder flipped) flipped

-- CR 800.4: "Unlike two-player games, multiplayer games can continue after one
-- or more players have left the game." CR 800.1: "A multiplayer game is a game
-- that begins with more than two players." GameState.turnOrder is the roster the
-- game BEGAN with and is never shortened (see Pawl.Type.GameState), so counting
-- seats answers "begins with" directly: a three-player game down to two
-- survivors still continues, and a rebuilt game (CR 727.1, CR 729.2) is seated
-- from the players who were in the game it came from and so answers for itself.
--
-- CR 800.4a's object removal is the one clause where this gate is OBSERVABLE
-- rather than merely vacuous. Inside a two-player game CR 104.2a ends the game
-- the moment a player leaves, so nothing there can see it -- but a two-player
-- SUBGAME is read after it ends: CR 729.5 has each player take the cards they
-- own in the subgame into their main-game library, and Setup.funnelBack does
-- that from the finished subgame's object pool. Removing the loser's cards would
-- destroy them.
--
-- A bare Bool, following Engine.skipsDraw: a proposition whose name is the
-- proposition, read by one `if`, with no third state to model. It names the
-- CAPABILITY rather than the cause, which is the seam M5.6b established.
continuesAfterDeparture :: GameState -> Bool
continuesAfterDeparture gs = length (GameState.turnOrder gs) > 2

-- CR 800.4a, first clause: "all objects (see rule 109) owned by that player
-- leave the game". Every id the player owns is deleted from GameState.objects
-- and from every collection that can name one -- the zones, the CR 725 exile
-- watch, and the parts of the combat record that stop meaning anything once
-- the id itself is gone.
--
-- Leaving the game is not a zone change, so this does not funnel through
-- Pawl.Event: no Moved event, no CR 616 replacement, no trigger. Pawl.Sba's
-- CR 704.5d token `ceaseToExist` is the same shape for the same reason.
--
-- Combat.blockers is DELIBERATELY left untouched -- neither a departing
-- BLOCKER's id inside an attacker's blocker set, nor a departing ATTACKER's own
-- key. CR 509.1h's last sentence: "A creature remains blocked even if all the
-- creatures blocking it are removed from combat" -- and
-- Damage.attackerAssignment's own comment says why the engine must honor that:
-- the recorded blockers set IS the record of blocked-ness, and the liveness
-- filter (Damage.onBattlefield) belongs at damage ASSIGNMENT, not here.
-- Deleting a departing blocker's id out of that set would make a blocked
-- attacker with no living blockers read as UNBLOCKED and send its full damage
-- at the defending player, which CR 510.1c forbids. A departing attacker's KEY
-- stays for the same reason and is filtered at the same place, by
-- Damage.blockerAssignment's CR 510.1d check on the attacker -- and that really
-- is the only correct site, because an attacker DESTROYED mid-combat (CR 510.4's
-- two-step window) leaves an identical stale key this function never sees.
--
-- Combat.attackers loses only the departing player's OWN entry, because a
-- deleted object cannot itself still be attacking. That much is cleanliness
-- rather than correctness: Damage.attackerAssignment's Projection.powerOf
-- already falls through to nothing for a missing id.
--
-- Three more things it deliberately does NOT touch, each because CR 800.4a
-- does not reach them:
--
--   * GameState.continuousEffects, GameState.replacements and
--     GameState.playerEffects. CR 109.1 lists what an object is -- "an ability on
--     the stack, a card, a copy of a card, a token, a spell, a permanent, or an
--     emblem" -- and a stored continuous effect is none of them, so the first
--     clause does not end one just because its source left. The effects this rule
--     DOES end are the control-granting ones, which is CR 800.4a's SECOND clause
--     and therefore controlEffectsEnd's job -- the next function in this file. A
--     departing player's Giant Growth on someone else's creature keeps its
--     +3/+3 until cleanup.
--
--   * GameState.delayedTriggers. A delayed triggered ability that has not
--     triggered is not on the stack, so by CR 109.1 it is not an object either.
--     It stays, it triggers, and CR 800.4d is what stops it reaching the stack --
--     which is exactly what CR 800.4d's own Astral Slide example describes. The
--     filter is in Engine.apnapPlayers.
--
--   * an exiledUntilMonarch entry whose VALUE is the departing player. That
--     effect survives its controller's departure: CR 800.4a ends only effects
--     which give that player control, and an exile grants none. Only an entry
--     whose KEY -- the exiled object itself -- is owned by the departing player is
--     dropped, because that object is leaving; whether the entry's value later
--     reads as "an opponent" of the new monarch is decided when
--     Pawl.Monarch.returnExiledForMonarch next runs, not here, under the
--     free-for-all reading recorded there: the players compete as individuals
--     (CR 806.1), so every other player is an opponent, and CR 102.3's teammates
--     are the one exception pawl has no way to reach (#175). NOT CR 102.2 --
--     this function only runs behind continuesAfterDeparture, so the game began
--     with more than two players (CR 800.1) and the two-player shortcut cannot
--     govern it.
objectsLeaveWith :: PlayerId -> GameState -> GameState
objectsLeaveWith pid gs =
  let owned = Map.keys (Map.filter (\obj -> Object.owner obj == pid) (GameState.objects gs))
      leave :: GameState -> ObjectId -> GameState
      leave g oid =
        let g1 = Game.removeFromZones pid oid g
            combat = GameState.combat g1
         in g1
              { GameState.objects = Map.delete oid (GameState.objects g1),
                GameState.combat =
                  combat
                    { Combat.attackers = Map.delete oid (Combat.attackers combat),
                      Combat.struckFirst = fmap (Set.delete oid) (Combat.struckFirst combat)
                    },
                GameState.exiledUntilMonarch = Map.delete oid (GameState.exiledUntilMonarch g1)
              }
   in List.foldl' leave gs owned

-- CR 800.4a, second clause: "any effects which give that player control of any
-- objects or players end."
--
-- Three carriers, because pawl has three ways to give control:
--
--   * a stored layer-2 SetController continuous effect (Master Thief, Act of
--     Treason -- both in the pool). Projection.givesControlTo makes the match, so
--     the case on Modification stays where it belongs.
--   * GameState.activeControl -- this turn's Decider (CR 723.1/723.3).
--   * GameState.pendingControl -- a Decider scheduled for a later turn
--     (CR 723.1b). CR 800.4b's last sentence, "If a player would be controlled by
--     a player who has left the game, they aren't", says the same thing one step
--     later at Engine.beginTurnOf's promotion. Two rules, one outcome; neither is
--     the other's spare.
--
-- CR 800.4a is immediate: "It happens as soon as the player leaves the game."
-- Master Thief's ForAsLongAs condition would eventually drop its effect through
-- Expiry.sweepConditional, but a sweep runs at the next settle, and the gap
-- between the two is exactly the difference between the rule and an
-- approximation of it.
controlEffectsEnd :: PlayerId -> GameState -> GameState
controlEffectsEnd pid gs =
  let heldBy decider = case decider of
        Decider.MkDecider d -> d == pid
   in gs
        { GameState.continuousEffects = filter (\eff -> not (Projection.givesControlTo pid eff)) (GameState.continuousEffects gs),
          GameState.activeControl = case GameState.activeControl gs of
            Just decider -> if heldBy decider then Nothing else Just decider
            Nothing -> Nothing,
          GameState.pendingControl = Map.filter (\decider -> not (heldBy decider)) (GameState.pendingControl gs)
        }

-- CR 800.4a, third clause: "Then, if that player controlled any objects on the
-- stack not represented by cards, those objects cease to exist." An activated or
-- triggered ability on the stack, or a token somehow there -- a SPELL is a card
-- and left with the first clause.
--
-- Empty by construction once the first two clauses have run, and the proof is
-- worth writing down rather than trusting: Projection.controllerOf is an object's
-- OWNER overridden by a layer-2 SetController and nothing else, the first clause
-- deleted every object the departing player owned, and the second deleted every
-- SetController naming them. Written anyway, so a second source of control could
-- not silently skip a clause of this rule -- nothing would warn.
nonCardStackObjectsCease :: PlayerId -> GameState -> GameState
nonCardStackObjectsCease pid gs =
  let notACard oid = case Game.lookupObject oid gs of
        Nothing -> False
        Just obj -> case Object.source obj of
          Source.OfCard _ -> False
          Source.OfToken _ -> True
          Source.OfAbility _ _ -> True
          Source.OfTrigger _ _ -> True
          Source.OfEmblem _ -> True
          Source.OfInherentTrigger _ _ -> True
      theirs oid = Projection.controllerOf oid gs == Just pid && notACard oid
      cease g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
           in g1 {GameState.objects = Map.delete oid (GameState.objects g1)}
   in List.foldl' cease gs (filter theirs (GameState.stack gs))

-- CR 800.4a, fourth clause: "Then, if there are any objects still controlled by
-- that player, those objects are exiled." CR 800.4a's third example is the case
-- it exists for -- Bribery's Serra Angel, owned by the player who stayed and
-- controlled by the one who left. Bribery is not in the pool.
--
-- CR 109.4: "Only objects on the stack or on the battlefield have a controller",
-- so the search need only cover the stack and the battlefield. It is NOT
-- "the stack was just swept, so the battlefield is the whole search" --
-- nonCardStackObjectsCease's sweep explicitly skips cards, so a controlled
-- SPELL would still be sitting on the stack if that were the only reason. The
-- real reason is unconditional: Object.owner is baked in at creation for
-- every kind of stack object and never mutated afterward -- Activate.hs:80
-- stamps the activating player (CR 113.8, activated ability), Engine.hs:321
-- stamps the triggering ability's controller (CR 113.8/603.3a, triggered
-- ability), Monarch.hs:138 stamps the inherent monarch trigger's controller
-- the same way, and a spell's owner is fixed when it is cast. Modification is
-- a flat sum with exactly one construction site for SetController
-- (Resolve.hs:880, Effect.GainControl, whose payload is always the granting
-- effect's source's controller at resolution). So once clause 1 has deleted
-- every object `pid` OWNS and clause 2 has ended every SetController naming
-- `pid` (regardless of which object it targets), Projection.controllerOf's
-- two cases -- owner, or the latest SetController naming this object -- can
-- both no longer read as `pid`, for any object of any kind, on the stack or
-- off it. The stack contributes nothing to this clause's search either way.
--
-- Empty by construction, for the reason just given -- which is also why
-- nonCardStackObjectsCease is empty. Written anyway, so a second source of
-- control could not silently skip a clause of this rule -- nothing would
-- warn. The exile is a direct move rather than an
-- Event.changeZone: this function is pure, so it cannot funnel, and a
-- leaves-the-battlefield trigger on this move is therefore not emitted (#179).
remainingControlledExiled :: PlayerId -> GameState -> GameState
remainingControlledExiled pid gs =
  let -- Projection.controls already hoists the control-grant list once rather
      -- than rebuilding it per battlefield object; free to take here too, even
      -- though this runs once per departure rather than in a hot loop.
      theirs = Projection.controls pid gs
      exileOne g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
              g2 = Game.insertIntoZone Zone.Exile (Object.owner obj) oid g1
           in g2 {GameState.objects = Map.insert oid obj {Object.zone = Zone.Exile} (GameState.objects g2)}
   in List.foldl' exileOne gs theirs

-- CR 104.2a: "A player still in the game wins the game if that player's opponents
-- have all left the game."
--
-- `gs` is the state AFTER the departures have been applied, so the survivors are
-- `stillPlaying gs`. `leaving` is who just left, and is needed only to tell
-- "nobody is playing because they all left at once" (a draw) from "nobody was
-- playing to begin with" (no result at all).
outcomeAfterLeaving :: [PlayerId] -> GameState -> Maybe Result
outcomeAfterLeaving leaving gs = case stillPlaying gs of
  [winner] -> Just (Result.Won winner)
  [] -> if null leaving then Nothing else Just Result.Drawn
  _ -> Nothing

-- CR 104.3a: leave the game IMMEDIATELY, and settle CR 104.2a right now rather
-- than at the next state-based action check -- which is the whole distinction
-- between 104.3a and 104.3b. An already-decided result is kept rather than
-- overwritten: CR 104.1 says the game already ended the moment a result was
-- set, and CR 104.2a's "overrides all effects that would preclude that player
-- from winning" describes the win itself, not a license to replace a result
-- the game already has. Pawl.Sba's pass settles its own outcome the same way.
leaveGame :: Departure -> PlayerId -> Game ()
leaveGame reason pid = State.modify' $ \gs ->
  let departed = depart reason pid gs
   in departed {GameState.result = GameState.result departed <|> outcomeAfterLeaving [pid] departed}
