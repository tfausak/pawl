-- CR 610.3: a zone change a card makes "until" a specified event, which is one
-- one-shot effect with a duration rather than a pair of abilities.
--
-- Two halves live here: the EVENT -- has the move's source left the battlefield?
-- -- which both the resolver's CR 610.3b gate and the sweep below ask, and the
-- SECOND ONE-SHOT EFFECT that rule 610.3 creates immediately after that event.
-- The register itself is GameState.movedUntilSourceLeaves, written by
-- Pawl.Engine.Resolve's MoveToZone arm.
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
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
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

-- | CR 610.3: perform the second one-shot effect for every watch whose source has
-- left the battlefield, returning each object to the zone it came from.
--
-- Runs in the settle loop, which is Pawl.Engine.Monarch.returnExiledForMonarch's
-- posture one rule over and for its reason: CR 704.3 makes "whenever a player
-- would get priority" the coarsest moment anything can observe the board, so
-- deciding at the departure and moving at the next settle is indistinguishable
-- from moving at the departure. What it is NOT is a triggered ability -- rule
-- 610.3 gives nobody a window to respond, and a return that used the stack could
-- be countered or removed -- see #2626.
--
-- CR 610.3d: the returns created after one event are one event too, so the due
-- watches are grouped by the event their source left in (the logged
-- GameEvent.Moved's group) and each group moves as one
-- (Event.changeZonesTogether), earlier departures first. Sources with no
-- logged departure return theirs together, after the rest.
-- data/scenarios/simultaneous-moves' Banisher Priest board proves one group's
-- return; the split across two departures is a REGRESSION FENCE, no board in the
-- suite removing two sources in separate events before one settle.
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
returnMoved :: Game Bool
returnMoved = do
  gs <- State.get
  let due =
        filter
          (\(_, watch) -> hasLeftTheBattlefield (ReturnWatch.source watch) gs)
          (Map.toList (GameState.movedUntilSourceLeaves gs))
      departedIn src =
        Foldable.foldl'
          ( \found logged -> case (found, LoggedEvent.event logged) of
              (Nothing, GameEvent.Moved m) | src `elem` Moved.departures m -> Just (LoggedEvent.group logged)
              _ -> found
          )
          Nothing
          (GameState.events gs)
      batches = Map.fromListWith (flip (<>)) [(maybe (Right ()) Left (departedIn (ReturnWatch.source watch)), [(oid, ReturnWatch.zone watch)]) | (oid, watch) <- due]
  if null due
    then pure False
    else do
      Monad.forM_ (Map.elems batches) $ \batch -> do
        _ <- Event.changeZonesTogether batch
        State.modify' (\g -> g {GameState.movedUntilSourceLeaves = foldr (Map.delete . fst) (GameState.movedUntilSourceLeaves g) batch})
      pure True
