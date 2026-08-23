-- CR 104.2a / 104.3: what happens when a player leaves the game.
--
-- Split out of Pawl.Engine.Sba because leaving is not always a state-based
-- action: CR 104.3b (life <= 0) arrives through the SBA pass, but CR 104.3a
-- (concede) is IMMEDIATE and Pawl.Engine.Engine reaches it directly.
--
-- The QUERY half -- Game.stillPlaying / Game.stillPlayingInOrder -- lives in
-- Pawl.Engine.Game instead. This module imports Pawl.Engine.Event, both directly
-- (CR 800.4a records events and files last known information of its own) and
-- through Pawl.Engine.Monarch (CR 725.4 reassignment happens inside `depart`),
-- so the event pipeline cannot reach the question through here.
module Pawl.Engine.Departure where

import Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Monarch as Monarch
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Types.ActiveReplacement as ActiveReplacement
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.Decider as Decider
import Pawl.Types.Departure (Departure)
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Player as Player
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ReplacementBucket as ReplacementBucket
import Pawl.Types.Result (Result)
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Status as Status
import qualified Pawl.Types.Zone as Zone

-- Mark a player as having left, with the reason they left, and perform
-- everything the rules attach to that moment.
--
-- MONADIC, and only the fourth clause needs it to be: CR 800.4a's exile is a
-- zone change like any other, so it goes through the zone-change funnel (CR
-- 400.7, CR 613.7d, CR 603.6c) rather than editing the zone maps. The other
-- three clauses stay pure functions on the state, composed here in the rule's
-- order.
--
-- CR 725.4 belongs INSIDE this function, not after it: the active player becomes
-- the monarch at the same time the player leaves. Both doors -- leaveGame (CR
-- 104.3a) and Pawl.Engine.Sba's fold (CR 704.5) -- get it by construction rather
-- than by remembering to call it.
depart :: Departure -> PlayerId -> Game ()
depart reason pid = do
  -- CR 800.4a's own ordering is load-bearing, so the clauses run in the rule's
  -- order. They run before the status flip, and THAT much is arbitrary: no
  -- clause reads Player.status, and continuesAfterDeparture reads
  -- GameState.turnOrder, which the flip does not touch. The flip's position is
  -- load-bearing only for the monarch call below, which
  -- Monarch.reassignOnDeparture requires to have already happened.
  --
  -- The status flip trailing the exile is load-bearing in one further way now
  -- that the exile emits events: CR 603.10a reads the board as it was
  -- immediately before the move, and CR 800.4d's filter on where a trigger may
  -- go is applied at CR 117.5, later than either.
  continues <- State.gets continuesAfterDeparture
  Monad.when continues $ do
    State.modify' (nonCardStackObjectsCease pid . controlEffectsEnd pid . objectsLeaveWith pid)
    remainingControlledExiled pid
  let lose p = p {Player.status = Status.Departed reason}
  State.modify' (\gs -> gs {GameState.players = Map.adjust lose pid (GameState.players gs)})
  State.modify' (\gs -> Monarch.reassignOnDeparture pid (Game.stillPlayingInOrder gs) gs)

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
-- Event.changeZoneAttaching: no Moved event and no CR 616 replacement --
-- Pawl.Engine.Sba's CR 704.5d token cease is the same shape for the same reason.
-- What it DOES borrow from that funnel is what CR 800.4a shares with a move: the
-- object ceases, so its CR 608.2h last known information is filed as it goes, and
-- a permanent's departure is recorded as a GameEvent.LeftTheGame so CR 603.6c's
-- second trigger event can be matched. Both are below.
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
-- More things it deliberately does NOT touch, each because CR 800.4a
-- does not reach them:
--
--   * the rows already in GameState.continuousEffects and
--     GameState.playerEffects, and every row of
--     GameState.replacements that grants nobody control. CR
--     109.1's list of what an object is does not include a stored continuous
--     effect, so the first clause does not end one just because its source left
--     -- a departing player's Giant Growth on someone else's creature keeps its
--     +3/+3 until cleanup. The effects this rule DOES end are the
--     control-granting ones, CR 800.4a's SECOND clause and so controlEffectsEnd's
--     job -- which is where the control-on-entry rows of GameState.replacements
--     are dropped (givesControlOnEntryTo), and only those.
--
--     GameState.continuousEffects is the one of the three this function WRITES,
--     and only ever by adding: `handover` below turns a departing permanent's
--     lingering static ability into a stored effect, which is CR 604.2's override
--     rather than anything CR 800.4a ends.
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
--     opponent" of the new monarch is decided at the next crowning, by
--     Pawl.Engine.Monarch.crown, under the
--     free-for-all reading recorded there (CR 806.1), whose one exception pawl
--     cannot reach is CR 102.3's teammates (#175). NOT CR 102.2 -- this function
--     only runs behind continuesAfterDeparture, so the game began with more than
--     two players (CR 800.1).
--
--   * a GameState.haunting entry whose VALUE is the departing player's permanent,
--     for the same reason and one rule over: CR 702.55b's link is not an effect
--     that gives anybody control, and rule 702.55b keeps naming the object the
--     haunt ability targeted after that object is gone. Only an entry whose KEY --
--     the haunting card itself -- belongs to the departing player is dropped,
--     because that card is leaving the game.
--
--   * a GameState.exiledWith entry whose VALUE is the departing player's
--     permanent, for haunting's reason a third time: CR 607.2a's link keeps
--     naming the object whose ability exiled the card after that object is gone,
--     which is the whole of what Hoarding Dragon's dies trigger reads. Only an
--     entry whose KEY -- the exiled card -- belongs to the departing player is
--     dropped.
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
                GameState.exiledUntilMonarch = Map.delete oid (GameState.exiledUntilMonarch g1),
                GameState.haunting = Map.delete oid (GameState.haunting g1),
                GameState.exiledWith = Map.delete oid (GameState.exiledWith g1)
              }
      -- CR 608.2h: each object ceases here, so this is the last moment its
      -- information is known -- the same five-part record
      -- Event.changeZoneAttaching files at the same point of a zone change, read
      -- from the same board and filed under the id the object had while it
      -- existed. Nothing mints a new incarnation for a departure, so that id is
      -- the only route back to what the object was.
      --
      -- Taken against `gs`, the board BEFORE any of them left, rather than
      -- against the fold's running state: rule 800.4a's first clause is one
      -- event, so a permanent's record must not read a board its siblings have
      -- already been removed from. Event.changeZoneInBatch's `asOf` is the same
      -- reading for the same reason.
      --
      -- Filed for every object the player owned, in every zone, exactly as the
      -- zone-change funnel files for every move -- the reader decides which ones
      -- it has a question about.
      filed oid = case Map.lookup oid (GameState.objects gs) of
        -- Unreachable: `owned` is drawn from GameState.objects itself.
        Nothing -> Nothing
        Just obj ->
          Just
            ( oid,
              LastKnown.MkLastKnown
                (Projection.project oid gs)
                -- CR 613.1b, and the reason this record is what a departure's
                -- trigger is read from: a permanent this player OWNED could
                -- have been controlled by somebody still in the game right up
                -- to the moment it left, and CR 603.3a hands that player its
                -- ability. The Object.owner fallback is unreachable for the
                -- reason Event.changeZoneAttaching gives at its own call.
                (Maybe.fromMaybe (Object.owner obj) (Projection.controllerOf oid gs))
                (Object.source obj)
                (Object.counters obj)
                (Event.copiedSnapshot oid gs)
                (Object.attachedTo obj)
            )
      -- CR 603.6c's second trigger event: "when a phased-in permanent leaves the
      -- game because its owner leaves the game". Only those, which is CR 702.26k
      -- saying the phased-out ones cause no zone-change ability to trigger --
      -- and membership of GameState.battlefield is exactly that distinction,
      -- since Pawl.Engine.Phasing takes a phased-out permanent out of that set
      -- and leaves its Object.zone alone.
      --
      -- Not a GameEvent.Moved: leaving the game reaches no zone (see
      -- Pawl.Types.GameEvent.LeftTheGame). Nothing else is recorded -- a card
      -- that was in a hand, a library, a graveyard, exile or on the stack leaves
      -- with its owner too, and no rule watches for that.
      --
      -- ONE event group for the batch (CR 800.4a is a single instant), so a
      -- look-back condition reads them as simultaneous rather than as a
      -- sequence.
      permanents = filter (\oid -> Set.member oid (GameState.battlefield gs)) owned
      -- CR 604.2's override, the other half of what this shares with a zone
      -- change: a permanent leaving the GAME has left the battlefield, so a card
      -- whose text says its effect continues anyway -- Titania's Song -- needs
      -- that effect handed over to GameState.continuousEffects as it goes.
      -- Event.lingeringHandover is the single writer of such an effect, shared
      -- with the zone-change funnel; borrowing it costs this module nothing the
      -- header warns about, since that function performs no move.
      --
      -- Nothing in CR 800.4a ends the handed-over effect: the second clause ends
      -- only effects giving the departing player CONTROL, which is the same
      -- reading that leaves their Giant Growth standing above. It expires on its
      -- own duration, at CR 514.2's cleanup for Titania's Song.
      --
      -- Read from `gs`, the board before any of them left, for `filed`'s reason:
      -- one event, so the effect handed over is the one that was applying while
      -- every one of these objects still existed.
      --
      -- Gated on `permanents` -- GameState.battlefield membership -- rather than
      -- on Object.zone, which is CR 702.26b: a phased-out permanent is treated as
      -- though it does not exist, so its static ability was generating no effect
      -- there is anything to continue. CR 702.26k still takes it out of the game
      -- with its owner, which is `owned` above.
      --
      -- The controller is read the way `filed` reads it, and for CR 109.5's
      -- reason: the arming's "you" is whoever controlled the permanent as it left,
      -- who need not be its owner -- the Object.owner fallback is unreachable for
      -- `filed`'s reason.
      handover =
        concatMap
          (\oid -> Event.lingeringHandover oid (Maybe.fromMaybe pid (Projection.controllerOf oid gs)) gs)
          permanents
      removed = List.foldl' leave gs owned
      recorded =
        removed
          { GameState.lastKnown = Map.fromList (Maybe.mapMaybe filed owned) <> GameState.lastKnown removed,
            GameState.continuousEffects = handover <> GameState.continuousEffects removed
          }
   in Event.simultaneouslyPure
        (\g -> List.foldl' (\g1 oid -> Event.recordEvent (GameEvent.LeftTheGame oid) g1) g permanents)
        recorded

-- CR 800.4a, second clause: any effects which give that player control of
-- objects or players end.
--
-- Four carriers, because pawl STORES control in four places:
--
--   * a stored layer-2 SetController continuous effect (Master Thief, Act of
--     Treason -- both in the pool). Projection.givesControlTo makes the match, so
--     the case on Modification stays where it belongs.
--   * GameState.activeControl -- this turn's Decider (CR 723.1/723.3).
--   * GameState.pendingControl -- a Decider scheduled for a later turn
--     (CR 723.1b). CR 800.4b says the same thing one step later, at
--     Engine.beginTurnOf's promotion; neither is the other's spare.
--   * a floating CR 616.1b control-on-entry replacement (Gather Specimens, the
--     pool's producer), whose whole content is that objects enter under CR
--     109.5's "you" -- the PlayerId this row baked when it was installed (see
--     Pawl.Types.ActiveReplacement's `controller`). Read as a bucket
--     (Replacement.bucketOfEffect, CR 616.1's own classification) rather than as
--     a rewrite constructor, so the rules core classifies and never identifies.
--
-- That fourth carrier is a reading of CR 800.4a rather than a quotation of it,
-- and the reading is worth writing down. The row gives its controller control of
-- objects that do not exist yet, so the alternative is that CR 800.4a's second
-- clause does not name it and CR 800.4b catches the entry instead. That loses
-- twice. CR 800.4b's sentences are the entry sentences -- a token is not created,
-- an object "remains in its current zone" -- and the second of those cannot be
-- expressed here at all: Event.runEntry materializes the entering permanent
-- BEFORE the CR 616.1 loop runs, so leaving a resolving creature spell on the
-- stack would mean undoing a completed move. And a surviving row stays a CR
-- 616.1b candidate, so a second row would put a CR 616.1b choice to the entering
-- permanent's controller between a live rewrite and one that does nothing --
-- Replacement.readsApplier answers True for this rewrite, so the pair is not
-- elided as indistinguishable. Ending the row forecloses both; CR 800.4b is left
-- untouched as the backstop for an entry that names a departed player for some
-- other reason (CR 800.4i's last known information, an instruction directed at
-- that player), which is where Event.createTokens' guard still stands.
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
-- Object.enteredUnder is a FIFTH place a player's name is recorded against an
-- object, and it is not a carrier at all -- it must never be swept here, however
-- much it looks like one. CR 110.2a's entry controller is the player who
-- controls the permanent BY DEFAULT (CR 110.2), which is the other side of the
-- line CR 800.4c draws, and no effect is giving it to them: the one-shot that
-- put the permanent there finished resolving. Clearing it would put the
-- permanent back under its owner and leave the fourth clause with nothing to
-- exile, which is the bug this engine had; Pawl.DepartureSpec's Meandering
-- Towershell case is what catches it coming back.
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
        { GameState.continuousEffects = filter (not . Projection.givesControlTo pid) (GameState.continuousEffects gs),
          GameState.activeControl = case GameState.activeControl gs of
            Just decider -> if heldBy decider then Nothing else Just decider
            Nothing -> Nothing,
          GameState.pendingControl = Map.filter (not . heldBy) (GameState.pendingControl gs),
          GameState.replacements = filter (not . givesControlOnEntryTo pid) (GameState.replacements gs)
        }

-- Is this floating row one of CR 800.4a's "effects which give that player
-- control of any objects"? Both halves are needed: the bucket, because a
-- departing player's Giant Growth or their damage shield grants nobody control
-- and CR 800.4a leaves it alone; and the baked controller, because a row
-- installed by a player still in the game is untouched however many opponents
-- leave.
--
-- Both halves are proved by test rather than asserted here. Pawl.ReplacementSpec
-- reddens on the CONTROLLER half through "CR 616.1b a departed player's row is
-- not a candidate, so carol is not asked" (drop the comparison and bob's
-- surviving row goes with alice's), and on the BUCKET half through "CR 800.4a a
-- departing player's shield is not a control effect, so it stays" (drop the
-- bucket check and a Mending Hands shield dies with its caster).
--
-- ActiveReplacement.controller is CR 109.5's "you" as of installation, which is
-- also the only player any current control-on-entry rewrite can name
-- (EntryRewrite.UnderSourceControl hands the permanent to the candidate's own
-- controller). A future rewrite in this bucket that named somebody else would
-- need the recipient re-derived here rather than this field read.
givesControlOnEntryTo :: PlayerId -> ActiveReplacement.ActiveReplacement -> Bool
givesControlOnEntryTo pid active =
  ActiveReplacement.controller active == pid
    && Replacement.bucketOfEffect (ActiveReplacement.effect active) == ReplacementBucket.ControlOnEntry

-- CR 800.4a, third clause: objects on the stack the departing player controlled
-- that are not represented by cards cease to exist -- an activated or triggered
-- ability, or a token somehow there. A SPELL is a card and left with the first
-- clause.
--
-- Empty by construction once the first two clauses have run, and the proof is
-- worth writing down rather than trusting. It is a proof about the objects THIS
-- clause reaches -- the non-cards -- and not about the stack as a whole:
-- remainingControlledExiled searches the stack too, because source 1 below can
-- answer `pid` for a stack object that IS a card.
--
-- Projection.controllerOf has THREE sources, and the claim is that after clauses
-- 1 and 2 none of them can answer `pid` for a surviving NON-CARD object on the
-- stack:
--
--   1. CR 110.2's DEFAULT controller (Projection.defaultControllerOf):
--      Object.enteredUnder where a rule recorded one, otherwise CR 108.4a's
--      Object.owner. Object.owner is baked at creation and never mutated, and
--      the first clause deleted every object `pid` owned. Object.enteredUnder is
--      written by three writers and no others (see Pawl.Types.Object):
--      Event.changeZoneAttaching for the effect that put a permanent onto the
--      battlefield (CR 110.2a), Pawl.Engine.Replacement's entry loop for a CR
--      616.1b rewrite of that, and Event.changeZoneCasting for the caster of a
--      spell (CR 405.4). Only the third writes a stack object, and what it
--      writes is a CARD being cast -- which `notACard` excludes -- so no object
--      this clause reaches can carry one. A battlefield permanent can
--      (Meandering Towershell returned under a thief who then leaves; Gather
--      Specimens' victim), and so can a spell cast off another player's card,
--      which is what clause 4 is for.
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
          Source.OfAbility _ -> True
          Source.OfTrigger _ -> True
          Source.OfEmblem _ -> True
          Source.OfInherentTrigger _ -> True
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
-- so the search covers those two and needs no others.
--
-- Both have real work to do, and for the one reason: source 1 is the DEFAULT
-- controller, and Object.enteredUnder can carry `pid` for an object `pid`
-- neither owns nor holds by any stored effect. On the battlefield that is a
-- Meandering Towershell a thief returned under their own control (CR 110.2a) or
-- Gather Specimens' victim (CR 616.1b); on the stack it is a spell `pid` cast off
-- a card somebody else owns (CR 405.4, Dire Fleet Daredevil). In both, clause 1
-- does not delete it (the owner is someone else) and clause 2 ends no effect
-- (there is none), so it arrives here still controlled by the departing player.
--
-- Two premises of nonCardStackObjectsCease's induction live here, at the sites
-- that would break them:
--
--   * Object.owner is baked in at creation for every kind of stack object and
--     never mutated afterward (CR 113.8, CR 603.3a), so clause 1's deletion is
--     exhaustive and stays so. Object.enteredUnder is the one thing that can make
--     an object read as controlled by a non-owner without a stored effect saying
--     so, and on the stack only a CAST writes it -- which is why that clause's
--     proof is about non-cards and this search is about everything.
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
-- pins.
--
-- Through Event.changeZoneInBatch, and that is the whole reason `depart` is
-- monadic: the exile is a permanent moving from the battlefield to exile, so CR
-- 400.7's new object, CR 613.7d's fresh timestamp, CR 608.2h's last known
-- information and CR 603.6c's leaves-the-battlefield triggers are all owed
-- here, and the funnel is where every one of them lives. A DEPARTING player's own such
-- trigger still never reaches the stack -- CR 603.3a makes them its controller
-- and CR 800.4d bars it -- so what the spec proves this with is a bystander's.
--
-- The victims are fixed from the board BEFORE the first move, so a permanent
-- this loop has already exiled cannot be re-derived into the list; the funnel's
-- own existence check is what drops one that a replacement effect moved
-- elsewhere in the meantime.
--
-- Not implemented: CR 800.4g's reassignment of a choice this move puts to the
-- departing player. The funnel's CR 616.1 loop asks the affected object's
-- controller, who here is the player leaving, so a board with two applicable
-- rewrites would put the race to them rather than to another player; no card in
-- `data/cards/` replaces a permanent being exiled, so no board reaches it
-- (#181).
--
-- IN BATCH and in ONE event group, against that same board, for the reason the
-- first clause files its last known information against it: "those objects are
-- exiled" is one event, so neither a member's CR 608.2h record nor its CR 616.1
-- candidate list may read a board its siblings have already left, and a CR
-- 603.10a look-back must read the group rather than a sequence.
remainingControlledExiled :: PlayerId -> Game ()
remainingControlledExiled pid = do
  gs <- State.get
  -- Projection.controls already hoists the control-grant list once rather than
  -- rebuilding it per battlefield object. The stack is short enough that the
  -- plain query is not worth a second hoist.
  let onStack = filter (\oid -> Projection.controllerOf oid gs == Just pid) (GameState.stack gs)
      theirs = Projection.controls pid gs <> onStack
  Event.simultaneously (Monad.mapM_ (\oid -> Event.changeZoneInBatch gs oid Zone.Exile) theirs)

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
leaveGame reason pid = do
  depart reason pid
  State.modify' (\departed -> departed {GameState.result = GameState.result departed <|> outcomeAfterLeaving [pid] departed})
