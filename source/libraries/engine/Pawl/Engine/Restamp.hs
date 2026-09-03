-- Covers CR 613.7m: the relative order of the timestamps several objects receive
-- at the same moment. One function, shared by every road that restamps a batch.
module Pawl.Engine.Restamp where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt

-- | CR 613.7m: the order a batch of objects receiving timestamps at the same
-- moment receives them in. The answer is the caller's fold order, so the first
-- named takes the EARLIER stamp.
--
-- Two keys, as the rule has two sentences. The primary is APNAP (CR 101.4) over
-- the seat each object belongs to, which is nobody's choice. The secondary is
-- that seat's own choice among the objects it holds, asked as one
-- Prompt.OrderTimestamps per group.
--
-- WHOSE a group is: the object's controller, or its owner where it has none, in
-- the rule's own words. Both readings are reachable here, since CR 110.2 gives
-- every permanent a controller while the batch is a list of ids the caller
-- swept.
--
-- The rule NAMES only the active player's choice and then says "followed by each
-- other player in turn order". Read as an ellipsis -- each seat orders its own
-- group -- rather than as leaving every other seat's internal order to the
-- engine, because the engine may not choose (and no rule elsewhere assigns that
-- order).
--
-- Asked at two or more, where the group is a real choice; a group of one is one
-- order. Game.permute keeps the engine's order for a non-permutation answer.
order :: [ObjectId] -> Game [ObjectId]
order oids = do
  gs <- State.get
  let apnap = Game.apnapOrder gs
      last_ = length apnap
      rank oid = maybe last_ (\pid -> Maybe.fromMaybe last_ (List.elemIndex pid apnap)) (seatOf gs oid)
      -- Ascending object id within a seat, so a batch nobody is asked about is
      -- still deterministic.
      groups = List.groupBy (\a b -> rank a == rank b) (List.sortOn (\oid -> (rank oid, oid)) oids)
      ask group = case group of
        first_ : _ : _ | Just pid <- seatOf gs first_ -> do
          answer <- Game.choose (Prompt.OrderTimestamps (Decide.deciderFor pid gs) pid group)
          pure (Game.permute group answer)
        _ -> pure group
  fmap concat (traverse ask groups)

-- Which seat an object belongs to for CR 613.7m: its controller (CR 110.2), and
-- its owner where it has none. Nothing only where the board holds neither, which
-- sorts the object last and asks nobody.
seatOf :: GameState -> ObjectId -> Maybe PlayerId
seatOf gs oid = case Projection.controllerOf oid gs of
  Just pid -> Just pid
  Nothing -> fmap Object.owner (Game.lookupObject oid gs)
