-- CR 104.2a / 104.3: what happens when a player leaves the game.
--
-- Split out of Pawl.Engine.Sba because leaving is not always a state-based
-- action: CR 104.3b (life <= 0) arrives through the SBA pass, but CR 104.3a
-- (concede) is IMMEDIATE and Pawl.Engine.Engine reaches it directly.
--
-- The QUERY half -- Game.stillPlaying / Game.stillPlayingInOrder -- lives in
-- Pawl.Engine.Game instead. This module imports Pawl.Engine.Monarch (CR 725.4
-- reassignment happens inside `depart`) and Monarch imports Pawl.Engine.Event,
-- so the event pipeline cannot reach the question through here.
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
-- CR 725.4 belongs INSIDE this function, not after it: the active player becomes
-- the monarch at the same time the player leaves. Both doors -- leaveGame (CR
-- 104.3a) and Pawl.Engine.Sba's fold (CR 704.5) -- get it by construction rather
-- than by remembering to call it.
depart :: Departure -> PlayerId -> GameState -> GameState
depart reason pid gs =
  let lose p = p {Player.status = Status.Departed reason}
      -- CR 800.4a's own ordering is load-bearing, so the clauses are composed in
      -- the rule's order. They run before the status flip, and THAT much is
      -- arbitrary: no clause reads Player.status, and continuesAfterDeparture
      -- reads GameState.turnOrder, which the flip does not touch. The flip's
      -- position is load-bearing only for the monarch call below, which
      -- Monarch.reassignOnDeparture requires to have already happened.
      settled =
        if continuesAfterDeparture gs
          then remainingControlledExiled pid (nonCardStackObjectsCease pid (controlEffectsEnd pid (objectsLeaveWith pid gs)))
          else gs
      flipped = settled {GameState.players = Map.adjust lose pid (GameState.players settled)}
   in Monarch.reassignOnDeparture pid (Game.stillPlayingInOrder flipped) flipped

-- CR 800.4: a multiplayer game can continue after players leave, and CR 800.1
-- makes "multiplayer" mean a game that BEGINS with more than two players.
-- GameState.turnOrder is the roster the game began with and is never shortened
-- (see Pawl.Types.GameState), so counting seats answers that directly: a
-- three-player game down to two survivors still continues, and a rebuilt game
-- (CR 727.1, CR 729.2) is seated from the game it came from.
--
-- CR 800.4a's object removal is the one clause where this gate is OBSERVABLE
-- rather than merely vacuous. Inside a two-player game CR 104.2a ends the game
-- the moment a player leaves, so nothing there can see it -- but a two-player
-- SUBGAME is read after it ends: CR 729.5 has each player take the cards they
-- own into their main-game library, and Setup.funnelBack does that from the
-- finished subgame's object pool. Removing the loser's cards would destroy them.
--
-- A bare Bool, following Engine.skipsDraw: a proposition read by one `if`, with
-- no third state to model, naming the CAPABILITY rather than the cause.
continuesAfterDeparture :: GameState -> Bool
continuesAfterDeparture gs = length (GameState.turnOrder gs) > 2

-- CR 800.4a, first clause: every object owned by the departing player leaves the
-- game. Every id they own is deleted from GameState.objects and from every
-- collection that can name one -- the zones, the CR 725 exile watch, and the
-- parts of the combat record that stop meaning anything once the id is gone.
--
-- Leaving the game is not a zone change, so this does not funnel through
-- Pawl.Engine.Event: no Moved event, no CR 616 replacement, no trigger --
-- Pawl.Engine.Sba's CR 704.5d token cease is the same shape for the same reason.
--
-- Combat.blockers is DELIBERATELY left untouched. Above all the KEY must stay:
-- it is the record of blocked-ness (Combat.isBlocked), and CR 509.1h keeps a
-- creature blocked even once every blocker is removed from combat. Dropping the
-- key would make a blocked attacker read as UNBLOCKED and send its full damage
-- at the defending player, which CR 510.1c forbids. The departing blocker's id
-- could be pruned from the set without changing an answer (Game.removeFromCombat
-- prunes exactly that way for CR 701.19a), but it is left alone because pruning
-- buys nothing: Damage's
-- liveness filter (Damage.onBattlefield) has to run at damage ASSIGNMENT
-- regardless, since a blocker DESTROYED mid-combat (CR 510.4's two-step window)
-- leaves an identical stale id this function never sees. A departing attacker's
-- key is left for the same reason, filtered by Damage.blockerAssignment's CR
-- 510.1d check.
--
-- Combat.attackers loses only the departing player's OWN entry, and that much is
-- cleanliness rather than correctness: Damage.attackerAssignment's
-- Projection.powerOf already falls through for a missing id. An entry naming a
-- deleted PLANESWALKER as its target (CR 306.6) is left alone and read the same
-- way -- Combat.stillAttacked asks the battlefield (CR 506.4).
--
-- Three more things it deliberately does NOT touch, each because CR 800.4a
-- does not reach them:
--
--   * GameState.continuousEffects, GameState.replacements and
--     GameState.playerEffects. CR 109.1's list of what an object is does not
--     include a stored continuous effect, so the first clause does not end one
--     just because its source left -- a departing player's Giant Growth on
--     someone else's creature keeps its +3/+3 until cleanup. The effects this
--     rule DOES end are the control-granting ones, CR 800.4a's SECOND clause and
--     so controlEffectsEnd's job.
--
--   * GameState.delayedTriggers. A delayed triggered ability that has not
--     triggered is not on the stack, so by CR 109.1 it is not an object either.
--     It stays, it triggers, and CR 800.4d is what stops it reaching the stack;
--     the filter is in Engine.apnapPlayers.
--
--   * an exiledUntilMonarch entry whose VALUE is the departing player. CR 800.4a
--     ends only effects which give that player control, and an exile grants
--     none. Only an entry whose KEY is owned by the departing player is dropped,
--     because that object is leaving; whether the value later reads as "an
--     opponent" of the new monarch is decided when
--     Pawl.Engine.Monarch.returnExiledForMonarch next runs, under the
--     free-for-all reading recorded there (CR 806.1), whose one exception pawl
--     cannot reach is CR 102.3's teammates (#175). NOT CR 102.2 -- this function
--     only runs behind continuesAfterDeparture, so the game began with more than
--     two players (CR 800.1).
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

-- CR 800.4a, second clause: any effects which give that player control of
-- objects or players end.
--
-- Three carriers, because pawl STORES control in three places:
--
--   * a stored layer-2 SetController continuous effect (Master Thief, Act of
--     Treason -- both in the pool). Projection.givesControlTo makes the match, so
--     the case on Modification stays where it belongs.
--   * GameState.activeControl -- this turn's Decider (CR 723.1/723.3).
--   * GameState.pendingControl -- a Decider scheduled for a later turn
--     (CR 723.1b). CR 800.4b says the same thing one step later, at
--     Engine.beginTurnOf's promotion; neither is the other's spare.
--
-- Object.enteredUnder is NOT a carrier and must never be swept here, however
-- much it looks like one: CR 110.2a's entry controller is the player who
-- controls the permanent BY DEFAULT (CR 110.2), which is the other side of the
-- line CR 800.4c draws, and no effect is giving it to them -- the one-shot that
-- put the permanent there finished resolving. Clearing it would put the
-- permanent back under its owner and leave the fourth clause with nothing to
-- exile, which is the bug this engine had; Pawl.DepartureSpec's Meandering
-- Towershell case is what catches it coming back.
--
-- Control has a FOURTH source that is deliberately absent from that list: a
-- layer-2 control-granting STATIC ability (Control Magic's
-- Modification.SetControllerToSource, CR 613.1b), which
-- Projection.controlGrants re-derives from the battlefield at every projection
-- and never stores. There is nothing here to filter, and filtering is not what
-- the rule wants either: CR 611.3a/611.3b keep a static ability's effect from
-- being locked in, and CR 109.5 makes its "you" the source's current controller.
-- So the effect stops giving the departing player control exactly when its
-- SOURCE stops being theirs, which the other clauses already do -- if they OWN
-- the source it left with the first clause, and if they merely CONTROLLED it,
-- the effect that gave them the source is a stored carrier this clause just
-- ended.
--
-- Card JSON could author a stored SetControllerToSource through
-- Effect.ModifyTarget, and this function does not end one -- but such an effect
-- is inert: Projection.controllerOfGiven's storedSetter matches only
-- Modification.SetController, Projection.controlGrants reads control-granting
-- static abilities off Face.staticAbilities and never off stored effects, and
-- Projection.applyModification's SetControllerToSource arm is the identity. A
-- card authoring one would grant control to no one, so there is nothing here for
-- CR 800.4a to end (#199).
--
-- CR 800.4a is immediate. Master Thief's ForAsLongAs condition would eventually
-- drop its effect through Expiry.sweepConditional, but a sweep runs at the next
-- settle, and the gap between the two is exactly the difference between the rule
-- and an approximation of it.
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

-- CR 800.4a, third clause: objects on the stack the departing player controlled
-- that are not represented by cards cease to exist -- an activated or triggered
-- ability, or a token somehow there. A SPELL is a card and left with the first
-- clause.
--
-- Empty by construction once the first two clauses have run, and the proof is
-- worth writing down rather than trusting. remainingControlledExiled reads it
-- for the STACK half of its own search -- but only that half, because source 1
-- below is where the two clauses stop asking the same question.
--
-- Projection.controllerOf has THREE sources, and the claim is that after clauses
-- 1 and 2 none of them can answer `pid` for a surviving object ON THE STACK:
--
--   1. CR 110.2's DEFAULT controller (Projection.defaultControllerOf):
--      Object.enteredUnder where CR 110.2a recorded one, otherwise CR 108.4a's
--      Object.owner. Object.owner is baked at creation and never mutated, and
--      the first clause deleted every object `pid` owned. Object.enteredUnder is
--      written only for a BATTLEFIELD entry -- by Event.changeZoneAttaching for
--      the effect that put the permanent there, and by
--      Pawl.Engine.Replacement's entry loop for a CR 616.1b rewrite of it -- and
--      Event.changeZone clears it on every move, so no STACK object can carry
--      one. A battlefield permanent can (Meandering Towershell returned under a
--      thief who then leaves; Gather Specimens' victim), which is what clause 4
--      is for.
--   2. a stored layer-2 SetController, whose PlayerId is BAKED at resolution
--      (CR 611.2c). The second clause deleted every stored effect whose payload
--      is `pid`, whichever object it affects.
--   3. a layer-2 control-granting STATIC ability
--      (Modification.SetControllerToSource, CR 613.1b), which
--      Projection.controlGrants re-derives from the battlefield. This one names
--      no player: by CR 109.5 its answer is controllerOf(source), the same
--      question one object further along.
--
-- Source 3 is therefore not a base case but a recursion, and the proof is an
-- induction over it. Projection.controllerOfGiven carries a visited set that
-- grows on every step and returns the object's OWNER when it revisits, so the
-- recursion terminates on a finite object pool, and every leaf it terminates on
-- is source 1, source 2, or a source that has itself been deleted (answering
-- Nothing) -- none of which can be `pid`. That covers the case clause 1 alone
-- does not: a departing player who CONTROLS a control-granting Aura they do not
-- OWN.
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

-- CR 800.4a, fourth clause: objects still controlled by the departing player are
-- exiled. CR 800.4a's third example is the case it exists for -- Bribery's Serra
-- Angel, owned by the player who stayed and controlled by the one who left.
-- Bribery is not in the pool.
--
-- CR 109.4 gives a controller only to objects on the stack or the battlefield,
-- so the search need only cover those two. The stack contributes nothing, and
-- NOT because "it was just swept" -- nonCardStackObjectsCease's sweep skips
-- cards, so a controlled SPELL would still be sitting there. The real reason is
-- that function's induction, which closes over the stack for spells too.
--
-- The BATTLEFIELD is a different matter, and this clause has real work to do
-- there: source 1 is CR 110.2's default controller, and Object.enteredUnder can
-- carry `pid` for a permanent `pid` neither owns nor holds by any stored effect
-- -- a Meandering Towershell a thief returned to the battlefield under their own
-- control (CR 110.2a), or Gather Specimens' victim (CR 616.1b), once that player
-- leaves. Clause 1 does not delete it (the owner is someone else) and clause 2
-- ends no effect (there is none), so it arrives here still controlled by the
-- departing player.
--
-- Two premises of that induction live here, at the sites that would break them:
--
--   * Object.owner is baked in at creation for every kind of stack object and
--     never mutated afterward (CR 113.8, CR 603.3a), so clause 1's deletion is
--     exhaustive and stays so. Object.enteredUnder is the one thing that can make
--     an object read as controlled by a non-owner without a stored effect saying
--     so, and it is unreachable on the stack.
--   * Clause 2 recognizes a control-granting effect by its PAYLOAD, not by where
--     it was built, so the NUMBER of construction sites is irrelevant and adding
--     one cannot weaken the induction. What the induction needs is that every
--     STORED layer-2 grant carries a baked player, so matching on the payload is
--     exhaustive over them. The one shape that would not is
--     SetControllerToSource, which carries no player and which card JSON could
--     reach through Effect.ModifyTarget; no card does, and Pawl.CardSpec lints
--     the pool to keep it that way (#199).
--
-- NOT empty by construction, unlike nonCardStackObjectsCease, and no longer
-- unreached: Pawl.DepartureSpec drives a Meandering Towershell stolen with
-- Control Magic through its own exile-and-return and then has its thief concede,
-- which is a permanent this clause exiles. Three or more seats are needed for
-- any of it -- at two, CR 104.2a ends the game the moment a player leaves and
-- continuesAfterDeparture skips all of CR 800.4a, which that spec's paired case
-- pins. The exile is a direct move rather than an Event.changeZone -- this
-- function is pure, so it cannot funnel, and a leaves-the-battlefield trigger on
-- this move is therefore not emitted (#179). It is still a move, so CR 400.7's
-- forgetting applies and Object.newIncarnation runs -- the part of the funnel
-- that needs no Game monad. CR 613.7d's fresh timestamp is the part that does,
-- and is not applied here (#179).
remainingControlledExiled :: PlayerId -> GameState -> GameState
remainingControlledExiled pid gs =
  let -- Projection.controls already hoists the control-grant list once rather
      -- than rebuilding it per battlefield object.
      theirs = Projection.controls pid gs
      exileOne g oid = case Game.lookupObject oid g of
        Nothing -> g
        Just obj ->
          let g1 = Game.removeFromZones (Object.owner obj) oid g
              g2 = Game.insertIntoZone Zone.Exile (Object.owner obj) oid g1
           in g2 {GameState.objects = Map.insert oid (Object.newIncarnation obj) {Object.zone = Zone.Exile} (GameState.objects g2)}
   in List.foldl' exileOne gs theirs

-- CR 104.2a: a player still in the game wins if their opponents have all left.
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
-- overwritten: CR 104.1 says the game already ended the moment a result was set,
-- and CR 104.2a's override describes the win itself, not a license to replace a
-- result the game already has. Pawl.Engine.Sba's pass settles the same way.
leaveGame :: Departure -> PlayerId -> Game ()
leaveGame reason pid = State.modify' $ \gs ->
  let departed = depart reason pid gs
   in departed {GameState.result = GameState.result departed <|> outcomeAfterLeaving [pid] departed}
