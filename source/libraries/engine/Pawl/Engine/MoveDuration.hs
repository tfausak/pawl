-- CR 610.3: a zone change a card makes "until" a specified event, which is one
-- one-shot effect with a duration rather than a pair of abilities.
--
-- Two halves live here: the EVENT -- has the duration's specified event
-- happened? -- which both the resolver's CR 610.3a/b gate and the sweep below
-- ask, and the SECOND ONE-SHOT EFFECT that rule 610.3 creates immediately after
-- that event, for every watch in GameState.movedUntil, written by
-- Pawl.Engine.Resolve's MoveToZone arm whatever the duration.
--
-- THE INVARIANT: the closed half. Nothing here reads which card or which effect
-- moved the object -- a MoveDuration is a classification the resolver hands over,
-- and the sweep sees only ids and zones.
module Pawl.Engine.MoveDuration where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import Pawl.Types.EventGroup (EventGroup)
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LeftTheGame as LeftTheGame
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import Pawl.Types.MoveDuration (MoveDuration)
import qualified Pawl.Types.MoveDuration as MoveDuration.Type
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.ReturnEnding (ReturnEnding)
import qualified Pawl.Types.ReturnEnding as ReturnEnding
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.Zone as Zone

-- | CR 610.3's specified event, as a question about the board: has this object
-- left the battlefield?
--
-- The object's own zone rather than membership of GameState.battlefield, and the
-- two differ for exactly one board: CR 702.26d says phasing changes no zone, so a
-- phased-out source has NOT left and the objects it moved stay where they are,
-- where the battlefield set excludes it (CR 702.26b).
--
-- An id GameState.objects no longer answers for HAS left: CR 400.7 deletes the
-- old incarnation and mints a new one at the destination, so a permanent that
-- left and came back is a different object and cannot re-arm a watch. That is
-- also what makes this exact for CR 610.3b, whose question is whether the event
-- has happened since the ability triggered.
hasLeftTheBattlefield :: ObjectId -> GameState -> Bool
hasLeftTheBattlefield oid gs = case Game.lookupObject oid gs of
  Nothing -> True
  Just obj -> Object.zone obj /= Zone.Battlefield

-- | CR 610.3a / 610.3b: has this duration's specified event already happened,
-- so the move it would make is declined? Asked by the resolver, of the
-- resolving object `resolving` and its `source` and `controller`, before it
-- gathers anything to move.
--
-- A crowning is an EVENT, so it is looked for in the log: a GameEvent.BecameMonarch
-- of an opponent of the controller, logged in the event group the resolving
-- object was put on the stack in or a later one (GameState.stackedIn). The
-- log is this turn's, and no stack object outlives a turn. An effect with no
-- stack object behind it has nothing to have happened since.
--
-- Not implemented: for a triggered ability, a crowning after it triggered but
-- before it was put on the stack -- CR 603.3's wait, in which only
-- state-based actions run (#4877).
--
-- data/scenarios/trigger/cr-610-3b-a-crowning-ahead-of-palace-jailers-exile-keeps-the-creature.json
-- is the proof: Jared Carthalion's trigger crowns bob ahead of Palace Jailer's
-- exile, and the creature stays.
hasHappened :: MoveDuration -> ObjectId -> ObjectId -> PlayerId -> GameState -> Bool
hasHappened duration resolving source controller gs = case duration of
  MoveDuration.Type.UntilSourceLeavesTheBattlefield -> hasLeftTheBattlefield source gs
  MoveDuration.Type.UntilAnOpponentBecomesTheMonarch -> case Map.lookup resolving (GameState.stackedIn gs) of
    Nothing -> False
    Just since ->
      any
        ( \logged -> case LoggedEvent.event logged of
            GameEvent.BecameMonarch pid -> LoggedEvent.group logged >= since && Game.areOpponents gs controller pid
            _ -> False
        )
        (GameState.events gs)

-- | The watch a move with this duration arms, for the move's source and the
-- effect's controller. CR 725's watch is armed undischarged whoever holds the
-- crown now, so an opponent who already holds it does not free the object:
-- Palace Jailer's ruling makes the ending a crowning, not a state.
endingOf :: MoveDuration -> ObjectId -> PlayerId -> ReturnEnding
endingOf duration source controller = case duration of
  MoveDuration.Type.UntilSourceLeavesTheBattlefield -> ReturnEnding.SourceLeaves source
  MoveDuration.Type.UntilAnOpponentBecomesTheMonarch ->
    ReturnEnding.OpponentCrowned MonarchWatch.MkMonarchWatch {MonarchWatch.controller = controller, MonarchWatch.due = Nothing}

-- | CR 610.3: perform the second one-shot effect for every "until" whose
-- specified event has happened, returning each object to the zone it came from.
-- A source-leaves watch is read off the board; a crowning watch is marked by
-- Pawl.Engine.Monarch.crown with the crowning's event group.
--
-- Runs in the settle loop: CR 704.3 makes "whenever a player would get
-- priority" the coarsest moment anything can observe the board, so deciding at
-- the event and moving at the next settle is indistinguishable from moving at
-- the event. What it is NOT is a triggered ability -- rule 610.3 gives nobody a
-- window to respond, and a return that used the stack could be countered or
-- removed -- see #2626.
--
-- CR 610.3d: the returns created after one event are one event too, whichever
-- ending they watched, so every due return is keyed by the event group of
-- its specified event -- the crowning's, or the logged departure of a moved
-- object's source (a GameEvent.Moved, or the GameEvent.LeftTheGame of CR
-- 800.4a) -- and each group moves as one (Event.changeZonesTogether), earlier
-- events first. Sources with no logged departure return theirs together, after
-- the rest. data/scenarios/simultaneous-moves' Banisher Priest and Palace
-- Jailer boards prove one register's group, and its concession board a
-- Jailer's and a Priest's prisoner returning as one. Pawl.LibraryOrderSpec's
-- "CR 610.3d a prisoner whose source left before a crowning returns before the
-- crowning's" proves the order across groups, at this sweep rather than in a
-- game (gap #4583).
--
-- The entry goes whether or not the move happened. A cancelled move (CR 614.6) or
-- an id that is no longer in the zone it was moved to has had its duration end all
-- the same, and rule 610.3 creates the second one-shot effect once.
--
-- CR 610.3c -- "returns under its owner's control" -- is the door's own answer
-- rather than a decision made here: the move names no controller, so the arrival
-- takes the rules' default and the object comes back to its owner. Pawl.BoardEffectSpec's Banisher Priest case fences that (a creature
-- exiled by another seat's ability comes back to its owner, not to the exiler),
-- but it is a fence and not a proof: a ReturnWatch records no controller, so the
-- other reading cannot be spelled here to mutate against.
--
-- Pawl.Engine.Event.leaveTheGame drops a watch whose KEY (the moved object)
-- leaves the game, never one whose source or controller does, so either
-- duration survives its controller's departure.
returnDue :: Game Bool
returnDue = do
  gs <- State.get
  let -- Nothing while the event has not happened; Just the group it happened
      -- in otherwise, which is Nothing for a source with no logged departure.
      dueIn watch = case ReturnWatch.ending watch of
        ReturnEnding.SourceLeaves source
          | hasLeftTheBattlefield source gs -> Just (departedIn source gs)
          | otherwise -> Nothing
        ReturnEnding.OpponentCrowned crowned -> fmap Just (MonarchWatch.due crowned)
      -- Left before Right, so a logged event's returns precede the unlogged.
      batches =
        Map.fromListWith
          (flip (<>))
          [ (maybe (Right ()) Left group, [(oid, ReturnWatch.zone watch)])
          | (oid, watch) <- Map.toList (GameState.movedUntil gs),
            Just group <- [dueIn watch]
          ]
      discharge g oid = g {GameState.movedUntil = Map.delete oid (GameState.movedUntil g)}
  if Map.null batches
    then pure False
    else do
      Monad.forM_ (Map.elems batches) $ \batch -> do
        _ <- Event.changeZonesTogether batch
        State.modify' (\g -> Foldable.foldl' discharge g (fmap fst batch))
      pure True

-- The event group a source left the battlefield in: the first logged
-- GameEvent.Moved that took it away, or CR 800.4a's GameEvent.LeftTheGame.
departedIn :: ObjectId -> GameState -> Maybe EventGroup
departedIn src gs =
  Foldable.foldl'
    ( \found logged -> case (found, LoggedEvent.event logged) of
        (Nothing, GameEvent.Moved m) | src `elem` Moved.departures m -> Just (LoggedEvent.group logged)
        (Nothing, GameEvent.LeftTheGame l) | LeftTheGame.object l == src -> Just (LoggedEvent.group logged)
        _ -> found
    )
    Nothing
    (GameState.events gs)
