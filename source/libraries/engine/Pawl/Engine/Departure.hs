-- CR 104.2a / 104.3: what happens when a player leaves the game.
--
-- Split out of Pawl.Engine.Sba because leaving is not always a state-based action. CR
-- 104.3b (life <= 0) is one and arrives through the SBA pass; CR 104.3a (concede)
-- is IMMEDIATE and Pawl.Engine.Engine reaches it directly. Having Engine call into
-- Pawl.Engine.Sba for something the rules say is not a state-based action would misstate
-- the rules in the module graph.
--
-- The QUERY half -- Game.stillPlaying / Game.stillPlayingInOrder, "who is still
-- in the game" -- lives in Pawl.Engine.Game instead. This module imports Pawl.Engine.Monarch
-- (CR 725.4 reassignment happens inside `depart`) and Monarch imports Pawl.Engine.Event,
-- so anything in the event pipeline that needs to ask the question cannot reach
-- it through here. Event.createTokens asks it for CR 800.4b.
module Pawl.Engine.Departure where

import Control.Applicative ((<|>))
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Decider as Decider
import Pawl.Types.Departure (Departure)
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Zone as Zone

-- Mark a player as having left, with the reason they left, and perform
-- everything the rules attach to that moment. Pure, because the CR 704.5 pass
-- folds it over several players before recomputing the outcome once.
--
-- CR 725.4 belongs INSIDE this function, not after it: "the active player becomes
-- the monarch at the same time as that player leaves the game." Both doors --
-- Departure.leaveGame (CR 104.3a) and Pawl.Engine.Sba's fold (CR 704.5) -- get it by
-- construction rather than by remembering to call it.
depart :: Departure -> PlayerId -> GameState -> GameState
depart reason pid gs =
  let lose p = p {Player.status = Status.Departed reason}
      -- CR 800.4a's own ordering ("Then ... Then ...") is load-bearing, so the
      -- clauses are composed in the rule's order and nothing here reorders them.
      -- They run before the status flip, and THAT much is arbitrary: no clause
      -- reads Player.status (Projection.controllerOf reads owners, stored
      -- continuous effects and the battlefield's static abilities -- see
      -- nonCardStackObjectsCease for the full list -- and none of those is a
      -- player's status), and continuesAfterDeparture reads GameState.turnOrder,
      -- which the flip does not touch. The flip's position is load-bearing only
      -- for the monarch call below, which Monarch.reassignOnDeparture requires
      -- to have already happened -- see its own haddock.
      settled =
        if continuesAfterDeparture gs
          then remainingControlledExiled pid (nonCardStackObjectsCease pid (controlEffectsEnd pid (objectsLeaveWith pid gs)))
          else gs
      flipped = settled {GameState.players = Map.adjust lose pid (GameState.players settled)}
   in Monarch.reassignOnDeparture pid (Game.stillPlayingInOrder flipped) flipped

-- CR 800.4: "Unlike two-player games, multiplayer games can continue after one
-- or more players have left the game." CR 800.1: "A multiplayer game is a game
-- that begins with more than two players." GameState.turnOrder is the roster the
-- game BEGAN with and is never shortened (see Pawl.Types.GameState), so counting
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
-- Pawl.Engine.Event: no Moved event, no CR 616 replacement, no trigger. Pawl.Engine.Sba's
-- CR 704.5d token `ceaseToExist` is the same shape for the same reason.
--
-- Combat.blockers is DELIBERATELY left untouched -- neither a departing
-- BLOCKER's id inside an attacker's blocker set, nor a departing ATTACKER's own
-- key. Above all the KEY must stay: it is the record of blocked-ness
-- (Combat.isBlocked), and CR 509.1h's last sentence -- "A creature remains
-- blocked even if all the creatures blocking it are removed from combat" -- says
-- the departure of every blocker cannot end it. Dropping the key would make a
-- blocked attacker read as UNBLOCKED and send its full damage at the defending
-- player, which CR 510.1c forbids.
--
-- The departing blocker's ID could be pruned from the set without changing an
-- answer -- Game.removeFromCombat prunes exactly that way for CR 701.19a -- but
-- it is left alone because it buys nothing: Damage's liveness filter
-- (Damage.onBattlefield) has to run at damage ASSIGNMENT regardless, since a
-- blocker DESTROYED mid-combat (CR 510.4's two-step window) leaves an identical
-- stale id this function never sees. A departing attacker's key is left for the
-- same reason and filtered at the same place, by Damage.blockerAssignment's
-- CR 510.1d check on the attacker.
--
-- Combat.attackers loses only the departing player's OWN entry, because a
-- deleted object cannot itself still be attacking. That much is cleanliness
-- rather than correctness: Damage.attackerAssignment's Projection.powerOf
-- already falls through to nothing for a missing id. An entry naming a deleted
-- PLANESWALKER as its target (CR 306.6) is left alone for the same reason and
-- read the same way -- Combat.stillAttacked asks the battlefield, so a
-- planeswalker that left the game with its owner stops being attacked without
-- this function touching the value (CR 506.4).
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
--     Pawl.Engine.Monarch.returnExiledForMonarch next runs, not here, under the
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
-- Three carriers, because pawl STORES control in three places:
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
-- Control has a FOURTH source that is deliberately absent from that list: a
-- layer-2 control-granting STATIC ability (Control Magic's
-- Modification.SetControllerToSource, CR 613.1b), which
-- Projection.controlGrants re-derives from the battlefield at every projection
-- and never stores. There is nothing here to filter, and filtering is not what
-- the rule wants either. CR 611.3b: a static ability's continuous effect
-- "applies at all times that the permanent generating it is on the
-- battlefield", and CR 611.3a: it "isn't 'locked in'". CR 109.5 fixes what it
-- says -- "For a static ability, [you] is the current controller of the object
-- it's on". So the effect stops giving the departing player control exactly
-- when its SOURCE stops being theirs, which the other clauses already do: if
-- they OWN the source it left with the first clause, and if they merely
-- CONTROLLED it, the effect that gave them the source is a stored carrier this
-- clause just ended, and the grant re-derives to the source's new controller
-- on the next projection. Card JSON could author a stored SetControllerToSource
-- through Effect.ModifyTarget, and this function does not end one -- but that is
-- not a gap: such a stored effect is inert. Projection.controllerOfGiven's
-- storedSetter matches only Modification.SetController (its wildcard drops
-- SetControllerToSource), Projection.controlGrants reads control-granting
-- static abilities off Card.staticAbilities and never off stored effects, and
-- Projection.applyModification's SetControllerToSource arm is the identity
-- (`pc`). A card authoring one would resolve, store the effect, and grant
-- control to no one; there is nothing here for CR 800.4a to end (#199).
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
-- worth writing down rather than trusting. It is stated once here, and
-- remainingControlledExiled reads it for the STACK half of its own search --
-- but only that half, because source 1 below is where the two clauses stop
-- asking the same question.
--
-- Projection.controllerOf has THREE sources, and the claim is that after
-- clauses 1 and 2 none of them can answer `pid` for any surviving object ON THE
-- STACK:
--
--   1. the object's CR 110.2 DEFAULT controller
--      (Projection.defaultControllerOf): Object.enteredUnder where a CR 616.1b
--      replacement recorded one, and otherwise CR 108.4a's Object.owner.
--      Object.owner is baked at creation and never mutated, and the first
--      clause deleted every object `pid` owned. Object.enteredUnder is written
--      only by Pawl.Engine.Replacement's entry loop and only for a BATTLEFIELD
--      entry, and Event.changeZone clears it on every move -- so no STACK
--      object can carry one, and this clause's search finds nothing. A
--      BATTLEFIELD permanent can (Gather Specimens takes an opponent's creature
--      and the taker then leaves), which is exactly what clause 4 is for.
--   2. a stored layer-2 SetController, whose PlayerId is BAKED at resolution
--      (CR 611.2c). The second clause deleted every stored effect whose payload
--      is `pid`, whichever object it affects, so no surviving effect has this
--      answer.
--   3. a layer-2 control-granting STATIC ability
--      (Modification.SetControllerToSource, CR 613.1b), which
--      Projection.controlGrants re-derives from the battlefield. This one names
--      no player: CR 109.5's "you" for a static ability is "the current
--      controller of the object it's on", so its answer is
--      controllerOf(source) -- the same question one object further along.
--
-- Source 3 is therefore not a base case but a recursion, and the proof is an
-- induction over it. Projection.controllerOfGiven carries a visited set that
-- grows on every step and returns the object's OWNER when it revisits, so the
-- recursion terminates on a finite object pool, and every leaf it terminates on
-- is source 1, source 2, or a source that has itself been deleted (which
-- answers Nothing, and Nothing is not Just `pid`) -- none of which can be
-- `pid`. Hence no object of any kind, on the stack or off it, still reads as
-- controlled by `pid`. That covers the case clause 1 alone does not: a
-- departing player who CONTROLS a control-granting Aura they do not OWN. The
-- Aura stays, but whatever made it theirs was source 2 or another source-3
-- step, and the induction closes both.
--
-- Written anyway, so a FOURTH source of control could not silently skip a
-- clause of this rule -- nothing would warn.
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
-- real reason is nonCardStackObjectsCease's induction, which closes over the
-- stack: none of Projection.controllerOf's three sources can answer `pid` for a
-- surviving stack object, spell or not. The stack contributes nothing to this
-- clause's search.
--
-- The BATTLEFIELD is a different matter, and this clause has real work to do
-- there: source 1 is CR 110.2's default controller, and Object.enteredUnder can
-- carry `pid` for a permanent `pid` neither owns nor holds by any stored effect
-- -- Gather Specimens' victim, once its taker leaves. Clause 1 does not delete
-- it (the owner is someone else) and clause 2 ends no effect (there is none),
-- so it arrives here still controlled by the departing player, which is CR
-- 800.4a's "any objects still controlled by that player" exactly.
--
-- Two premises of that induction live here, at the sites that would break them:
--
--   * Object.owner is baked in at creation for every kind of stack object and
--     never mutated afterward -- Activate.hs stamps the activating player
--     (CR 113.8, activated ability), Engine.hs stamps the triggering ability's
--     controller (CR 113.8/603.3a, triggered ability), Monarch.hs stamps the
--     inherent monarch trigger's controller the same way, and a spell's owner
--     is fixed when it is cast. No later write can turn an object `pid` does
--     not own into one they do, so clause 1's deletion is exhaustive and stays
--     so. Object.enteredUnder is the one thing that can make an object read as
--     controlled by someone who does not own it without a stored effect saying
--     so, and it is unreachable on the stack for the reason given there.
--   * Clause 2 recognizes a control-granting effect by its PAYLOAD, not by
--     where it was built: Projection.givesControlTo asks whether a stored
--     SetController names `pid`, so the NUMBER of construction sites is
--     irrelevant and adding one cannot weaken the induction. (Do not read this
--     as "there is only one site" -- Resolve.hs's Effect.GainControl arm bakes
--     the source's controller at resolution per CR 611.2c, and Codec.hs also
--     builds one when decoding card JSON.) What the induction needs is
--     narrower: that every STORED layer-2 grant carries a baked player, so
--     matching on the payload is exhaustive over them. The one shape that
--     would not is SetControllerToSource, which carries no player and which
--     card JSON could reach through Effect.ModifyTarget; no card does, and
--     Pawl.CardSpec lints the pool to keep it that way (#199).
--
-- NOT empty by construction, unlike nonCardStackObjectsCease: it was until
-- Object.enteredUnder existed, and the comment that said so was the reason to
-- write the clause anyway rather than assume it away -- a further source of
-- control could not silently skip a clause of this rule, and nothing would have
-- warned. No card reaches it yet, since the only board that produces one needs
-- three or more seats (at two, CR 104.2a ends the game the moment a player
-- leaves and continuesAfterDeparture skips all of CR 800.4a) and a Gather
-- Specimens whose controller then leaves (#582). The exile is a direct move rather than an
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
-- `Game.stillPlaying gs`. `leaving` is who just left, and is needed only to tell
-- "nobody is playing because they all left at once" (a draw) from "nobody was
-- playing to begin with" (no result at all).
outcomeAfterLeaving :: [PlayerId] -> GameState -> Maybe Result
outcomeAfterLeaving leaving gs = case Game.stillPlaying gs of
  [winner] -> Just (Result.Won winner)
  [] -> if null leaving then Nothing else Just Result.Drawn
  _ -> Nothing

-- CR 104.3a: leave the game IMMEDIATELY, and settle CR 104.2a right now rather
-- than at the next state-based action check -- which is the whole distinction
-- between 104.3a and 104.3b. An already-decided result is kept rather than
-- overwritten: CR 104.1 says the game already ended the moment a result was
-- set, and CR 104.2a's "overrides all effects that would preclude that player
-- from winning" describes the win itself, not a license to replace a result
-- the game already has. Pawl.Engine.Sba's pass settles its own outcome the same way.
leaveGame :: Departure -> PlayerId -> Game ()
leaveGame reason pid = State.modify' $ \gs ->
  let departed = depart reason pid gs
   in departed {GameState.result = GameState.result departed <|> outcomeAfterLeaving [pid] departed}
