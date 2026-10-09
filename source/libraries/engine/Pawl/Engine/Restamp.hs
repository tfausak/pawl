-- Covers CR 613.7m: the relative order of the timestamps several objects receive
-- at the same moment. One function, shared by every road that restamps a batch.
module Pawl.Engine.Restamp where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event.Trigger as Trigger
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.EnteringTogether as EnteringTogether
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import Pawl.Types.Timestamp (Timestamp)
import qualified Pawl.Types.Zone as Zone

-- | CR 613.7m: the order a batch of objects receiving timestamps at the same
-- moment receives them in. The RESULT is the caller's fold order, so whatever
-- comes back first takes the EARLIER stamp.
--
-- Two keys, as the rule has two sentences. The primary is APNAP (CR 101.4) over
-- the seat each object belongs to, which is nobody's choice. The secondary is
-- that seat's own choice among the objects it holds, asked as one
-- Prompt.OrderTimestamps per group.
--
-- WHOSE a group is: the object's controller, or its owner where it has none,
-- which is the rule's own parenthetical. CR 110.2 gives every permanent a
-- controller, so the owner branch is reached only by an id the battlefield no
-- longer holds (CR 400.7) -- which the writers then decline to restamp anyway.
--
-- The rule NAMES only the active player's choice and then says "followed by each
-- other player in turn order". Read as an ellipsis -- each seat orders its own
-- group -- rather than as leaving every other seat's internal order to the
-- engine, because the engine may not choose (and no rule elsewhere assigns that
-- order).
--
-- Asked at two or more, on the count alone, as Prompt.OrderTriggers' CR 101.4c
-- caller is: whether the relative stamp is ever read depends on what applies in
-- the layer afterwards, which this cannot know. A group of one is one order.
-- Game.permute keeps the engine's order for a non-permutation answer.
--
-- Proved by Pawl.RestampSpec, whose two boards separate the keys: the seat's own
-- choice on the transform road, and APNAP across two seats on the nightfall one.
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

-- | CR 712.21b / 730.3b: the exception to `order` -- ONE player, the one who
-- exiled a melded or merged permanent, orders the cards it became, which have
-- just been put into exile with stamps from `start` on. Permutes those stamps in
-- place (`reassign`); asked at two or more, filtered by Game.permute.
orderFor :: PlayerId -> Timestamp -> [ObjectId] -> Game ()
orderFor pid start arrivals = case arrivals of
  _ : _ : _ -> do
    gs <- State.get
    answer <- Game.choose (Prompt.OrderTimestamps (Decide.deciderFor pid gs) pid arrivals)
    State.modify' (reassign start (Game.permute arrivals answer))
  _ -> pure ()

-- | CR 613.7m over a batch that has ALREADY entered the battlefield (CR 613.7d)
-- at one moment: `arrivals` in the order they arrived, `start` the first stamp
-- the batch could have minted. Asked AFTER the arrivals rather than before them,
-- as `order`'s other callers ask, because what an arrival is -- its controller
-- (CR 110.2a, CR 616.1b's rewrite), its host (CR 303.4f), the object a copy
-- entry chose (CR 707.5) -- is settled only as it enters.
--
-- An in-place PERMUTATION of the stamps the batch minted, never fresh ones, so
-- nothing outside the batch moves relative to it. Each arrival carries the stamps
-- its own entry minted with it -- its counters' (CR 613.7c) and the stored
-- effects it sources -- since those were received as it entered and follow it.
-- The canonical answer is the arrival order itself, which leaves every stamp
-- where it was.
--
-- Battlefield arrivals only. Elsewhere an object's stamp is read by
-- Quantity.lastCardExiledWith, whose reader (Duplicant) links one target per
-- exile, so its only simultaneous batch is CR 712.21b's, which `orderFor` asks;
-- and by its own static abilities functioning there (CR 113.6, CR 613.7a), and no
-- printing's such ability writes a characteristic an order could change (MTGJSON
-- 2026-08-23, text "As long as/While ... is in a graveyard/in exile/in your
-- hand/in the command zone" beside a set base power, lost abilities, a set type
-- or a set color: no hit). A card whose graveyard ability set such a value would
-- refute that.
--
-- Proved by Pawl.RestampSpec's Replenish boards (Humility and Opalescence), on
-- Pawl.Engine.Resolve's MoveToZone road, and its Rite of Replication board on
-- the token road, which Event.together settles for Effect.CreateCopy.
-- Pawl.Engine.MoveDuration.returnDue reaches it too, and no board observes
-- that (gap #4214).
--
-- Inside a CR 608.2f action (Event.together) the batch is only noted, and the
-- action's whole batch is settled once as it ends: everything it put onto the
-- battlefield entered at one moment, whichever instruction or iteration did it.
-- Proved by Pawl.RestampSpec's Mirror Match board, and on the conjure road by
-- its Ornate Imitations board.
settle :: Timestamp -> [ObjectId] -> Game ()
settle start arrivals = do
  gs <- State.get
  case GameState.enteringTogether gs of
    Just batch
      | null arrivals -> pure ()
      | otherwise -> State.put gs {GameState.enteringTogether = Just batch {EnteringTogether.arrivals = EnteringTogether.arrivals batch <> Seq.fromList arrivals}}
    Nothing -> do
      ordered <- order (filter (\oid -> fmap Object.zone (Game.lookupObject oid gs) == Just Zone.Battlefield) arrivals)
      State.modify' (reassign start ordered)
      -- CR 603.10: the board just after the entry is the one under the chosen
      -- stamps. data/scenarios/restamp's "an enters trigger reads the chosen
      -- stamps" pair (Kiora beside Replenish's Humility) proves it.
      State.modify' (\after -> foldr Trigger.resampleEntry after ordered)

-- The permutation `settle` asks for: the batch's stamps, pooled and sorted, dealt
-- out again block by block in `ordered`, each block in its own old order.
reassign :: Timestamp -> [ObjectId] -> GameState -> GameState
reassign start ordered gs =
  let objects = GameState.objects gs
      effects = GameState.continuousEffects gs
      fresh ts = ts >= start
      blockOf oid =
        let own = foldMap (\obj -> Object.timestamp obj : filter fresh (Map.elems (Object.counterTimestamps obj))) (Map.lookup oid objects)
            sourced = [ContinuousEffect.timestamp e | e <- effects, ContinuousEffect.source e == oid, fresh (ContinuousEffect.timestamp e)]
         in Set.toAscList (Set.fromList (own <> sourced))
      -- A stamp two blocks share -- one an instruction minted for all of them --
      -- stays with the first, so the pool deals each stamp out once.
      dealt = snd (List.foldl' (\(seen, acc) ts -> if Set.member ts seen then (seen, acc) else (Set.insert ts seen, acc <> [ts])) (Set.empty, []) (concatMap blockOf ordered))
      mapping = Map.fromList (zip dealt (List.sort dealt))
      remap ts = Map.findWithDefault ts ts mapping
      member = (`elem` ordered)
      -- Not implemented: CR 613.7k's sticker restamp under this reorder, and
      -- under a new timestamp without a zone change (CR 613.7e-g); unobservable
      -- while stickers are art (#872).
      restampObject oid obj
        | member oid = obj {Object.timestamp = remap (Object.timestamp obj), Object.counterTimestamps = fmap remap (Object.counterTimestamps obj)}
        | otherwise = obj
      restampEffect e
        | member (ContinuousEffect.source e) = e {ContinuousEffect.timestamp = remap (ContinuousEffect.timestamp e)}
        | otherwise = e
   in gs {GameState.objects = Map.mapWithKey restampObject objects, GameState.continuousEffects = fmap restampEffect effects}

-- Which seat an object belongs to for CR 613.7m: its controller (CR 110.2), and
-- its owner where it has none. Nothing only where the board holds neither, which
-- sorts the object last and asks nobody.
seatOf :: GameState -> ObjectId -> Maybe PlayerId
seatOf gs oid = case Projection.controllerOf oid gs of
  Just pid -> Just pid
  Nothing -> fmap Object.owner (Game.lookupObject oid gs)
