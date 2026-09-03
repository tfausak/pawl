-- Pattern matching on Pawl.Types.Prompt, a GADT, in the answerers below.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Restamp: CR 613.7m's order over the timestamps two or more
-- objects receive at one moment -- APNAP (CR 101.4) across the seats, and each
-- seat's own choice among the objects it holds, asked as Prompt.OrderTimestamps.
--
-- Two of the three roads are driven, one per key. Pawl.Engine.Resolve's
-- turnPermanentsOver hands its swept victims to Restamp.order before the fold
-- that mints CR 613.7g's per-permanent stamps, and that is where the seat's own
-- choice is proved; Pawl.Engine.Daytime's turnDue is where APNAP across two seats
-- is. The third, Pawl.Engine.Resolve's Effect.TurnFaceDown arm (CR 613.7f),
-- reaches the same function and no board can observe it -- see the note on
-- restampOrderSpec.
module Pawl.RestampSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Text as Text
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Daytime as Daytime
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Restamp" $ do
  restampOrderSpec s registry
  apnapOrderSpec s registry

-- | The producer is a synthetic pair, and no printing reaches the rule; see
-- #2571 for the search behind that. Observing which of two simultaneous CR
-- 613.7g stamps is later needs two double-faced permanents whose BACK faces both
-- write the same third object in one layer, transformed by one instruction.
-- Synthetic Rampart Warden // Synthetic Rampart Sculptor and Synthetic Rampart
-- Keeper // Synthetic Rampart Shaper are that pair: Human Clerics and Wizards in
-- front, so Moonmist's "transform all Humans" takes both at once, and behind
-- them two layer-7b abilities that set every Wall's base power and toughness --
-- 5/5 and 3/3, read off a Wall of Stone that prints 0/8, so the three values
-- tell each other apart.
--
-- The Warden is placed FIRST, so the engine's own canonical order within alice's
-- group (ascending object id) stamps the Keeper's 3/3 later. That is what makes
-- the answer observable: the two cases below differ in exactly one thing, the
-- permutation alice answers with.
--
-- The face-down road reaches Restamp.order too and nothing drives it: CR 708.2a
-- leaves a face-down permanent no abilities of its own, so nothing a permanent
-- turned face down by Effect.TurnFaceDown writes can be read back, and mutating
-- that call site leaves the suite green. CR 702.145c's nightfall sweep is the
-- third road and has its own board below, which cannot be this one -- a daybound
-- permanent refuses an instruction's transform (CR 702.145b).
restampOrderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
restampOrderSpec s registry =
  Spec.describe s "OrderTimestamps" $ do
    -- The gameplay-level assertion is the Wall's power: it is whichever back face
    -- was stamped LATER, so it reads 5 only if alice's answer moved the Warden
    -- behind the Keeper.
    Spec.it s "CR 613.7m the seat's own answer decides which simultaneous stamp is later" $ do
      (board, wardenId, keeperId, wallId) <- rampartBoard s registry True
      let after = transformAll reversingAnswer board
      Spec.assertEqWith s "CR 613.7m the Sculptor's 5/5 was stamped last, so it defines the Wall" (S.powerToughnessOf wallId after) (Just (5, 5))
      Spec.assertEqWith s "both Humans transformed" (faceNames [wardenId, keeperId] after) [Just "Synthetic Rampart Sculptor", Just "Synthetic Rampart Shaper"]
      Spec.assertEqWith s "and alice was asked once, over both of her permanents" (orderPrompts board) [(S.alice, 2)]
    -- THE PAIR with the case above, differing only in the answer: the engine's
    -- canonical order stands when alice takes it, and it is the other value.
    Spec.it s "CR 613.7m the canonical order stands when the seat answers with it" $ do
      (board, _, _, wallId) <- rampartBoard s registry True
      let after = transformAll S.castAnswer board
      Spec.assertEqWith s "CR 613.7m the Shaper's 3/3 was stamped last, so it defines the Wall" (S.powerToughnessOf wallId after) (Just (3, 3))
    -- One permanent is one order, so there is nothing to ask. The board differs
    -- from the two above in exactly one thing -- whether the Warden is on the
    -- battlefield.
    Spec.it s "CR 613.7m a lone restamped permanent raises no order question" $ do
      (board, _, keeperId, wallId) <- rampartBoard s registry False
      let after = transformAll reversingAnswer board
      Spec.assertEqWith s "not asked" (orderPrompts board) []
      Spec.assertEqWith s "the Keeper transformed anyway and its 3/3 defines the Wall" (S.powerToughnessOf wallId after) (Just (3, 3))
      Spec.assertEqWith s "off its own back face" (faceNames [keeperId] after) [Just "Synthetic Rampart Shaper"]

-- | CR 613.7m's PRIMARY key, which is nobody's choice: the seats are ordered
-- APNAP (CR 101.4), so the active player's permanents take the earlier stamps
-- whatever order the board enumerates them in. One permanent per seat, so no
-- ordering question is raised and the sort alone decides.
--
-- The road is Pawl.Engine.Daytime's turnDue, which enumerates
-- GameState.battlefield ascending and reaches Pawl.Engine.Game.turnFaceOver
-- without an instruction: CR 702.145c turns every front-face-up daybound
-- permanent over the moment it becomes night, so one nightfall restamps both
-- seats' permanents at once. bob's is placed FIRST, so ascending object id and
-- APNAP order disagree about which of the two is stamped later.
--
-- The producers are a second synthetic pair, daybound where the Rampart pair is
-- not: CR 702.145b forbids an instruction from transforming a daybound
-- permanent, so no board can drive both roads with one pair.
apnapOrderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
apnapOrderSpec s registry =
  Spec.describe s "Apnap" $ do
    Spec.it s "CR 613.7m the non-active player's simultaneous stamp is the later one" $ do
      (board, hunterId, watcherId, wallId) <- bulwarkBoard s registry
      let night = nightfall board
      Spec.assertEqWith s "CR 613.7m bob is not the active player, so his Stalker's 4/4 was stamped last" (S.powerToughnessOf wallId night) (Just (4, 4))
      Spec.assertEqWith s "it became night" (GameState.daytime night) (Just Daytime.Night)
      Spec.assertEqWith s "and CR 702.145c turned both permanents over" (faceNames [hunterId, watcherId] night) [Just "Synthetic Bulwark Howler", Just "Synthetic Bulwark Stalker"]
      Spec.assertEqWith s "with nobody asked to order a group of one" (nightPrompts board) []

-- bob's daybound Watcher is placed first and alice's Hunter after it, so the
-- battlefield enumerates bob's permanent first; a Wall of Stone reads whichever
-- back face was stamped later. Settled, which is where CR 702.145d makes it day.
bulwarkBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
bulwarkBoard s registry = do
  hunter <- S.printingOf s registry "Synthetic Bulwark Hunter"
  watcher <- S.printingOf s registry "Synthetic Bulwark Watcher"
  wall <- S.printingOf s registry "Wall of Stone"
  let (watcherId, g1) = S.addCreature watcher S.bob (Setup.emptyGame S.bothPlayers)
      (hunterId, g2) = S.addCreature hunter S.alice g1
      (wallId, g3) = S.addCreature wall S.alice g2
  pure (S.runPure S.identityAnswer g3 Engine.settleForPriority, hunterId, watcherId, wallId)

-- CR 502.2's day/night check in the untap step, off a board whose previous turn
-- saw no spells: day becomes night, and CR 702.145c turns both daybound
-- permanents over as it does.
nightfall :: GameState.GameState -> GameState.GameState
nightfall gs =
  S.runPure
    S.identityAnswer
    gs {GameState.spellsCastLastTurn = 0}
    (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))

-- Whether the nightfall sweep asked anybody CR 613.7m's order.
nightPrompts :: GameState.GameState -> [(PlayerId.PlayerId, Int)]
nightPrompts gs =
  let recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Int)] r
      recording p = case p of
        Prompt.OrderTimestamps _ pid batch -> do
          State.modify (<> [(pid, length batch)])
          pure (S.identityAnswer p)
        _ -> pure (S.identityAnswer p)
   in State.execState (Engine.runGame recording gs {GameState.spellsCastLastTurn = 0} (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))) []

-- alice, the active player, controls the Keeper, a Wall of Stone and -- when
-- `both` -- the Warden ahead of it, plus the two Forests Moonmist costs, with
-- Moonmist in hand. Nobody else holds a Human, so the sweep's APNAP grouping
-- leaves one group and only the intra-seat key is left to decide.
rampartBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Bool -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId)
rampartBoard s registry both = do
  warden <- S.printingOf s registry "Synthetic Rampart Warden"
  keeper <- S.printingOf s registry "Synthetic Rampart Keeper"
  wall <- S.printingOf s registry "Wall of Stone"
  moonmist <- S.printingOf s registry "Moonmist"
  forest <- S.printingOf s registry "Forest"
  let (wardenId, g1) = S.addCreature warden S.alice (S.landsInPlay forest 2)
      (keeperId, g2) = S.addCreature keeper S.alice (if both then g1 else S.landsInPlay forest 2)
      (wallId, g3) = S.addCreature wall S.alice g2
      (g4, _) = S.handOne moonmist g3
  pure (g4, wardenId, keeperId, wallId)

-- alice casts the Moonmist in her hand and it resolves. The spell is the last
-- card put into her hand, so castAnswer's cast-else-play interpreter finds it.
transformAll :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
transformAll answer gs =
  let cast = S.runPure answer gs (S.cast S.alice (lastInHand gs))
   in S.runPure answer cast Stack.resolveTop

-- The Moonmist: the only card in alice's hand.
lastInHand :: GameState.GameState -> ObjectId.ObjectId
lastInHand gs = case Game.zoneMembers Zone.Hand S.alice gs of
  oid : _ -> oid
  [] -> ObjectId.MkObjectId 0

-- CR 613.7m answered with the REVERSE of the offered indices, so the permanent
-- the engine's canonical order put first takes the later stamp. Pinned by index
-- rather than by name: an answerer that searched for the Warden would repair the
-- assertion after a mutation.
reversingAnswer :: Prompt.Prompt r -> r
reversingAnswer p = case p of
  Prompt.OrderTimestamps _ _ batch -> reverse (zipWith const [0 ..] batch)
  _ -> S.castAnswer p

-- Who was asked CR 613.7m's order, and over how many permanents. Threaded
-- through State rather than answered purely, since "not asked" is otherwise
-- invisible to the answerer.
orderPrompts :: GameState.GameState -> [(PlayerId.PlayerId, Int)]
orderPrompts gs =
  let recording :: Prompt.Prompt r -> State.State [(PlayerId.PlayerId, Int)] r
      recording p = case p of
        Prompt.OrderTimestamps _ pid batch -> do
          State.modify (<> [(pid, length batch)])
          pure (S.castAnswer p)
        _ -> pure (S.castAnswer p)
   in State.execState (Engine.runGame recording gs (S.cast S.alice (lastInHand gs) >> Stack.resolveTop)) []

-- The face each of these permanents is showing, as a name.
faceNames :: [ObjectId.ObjectId] -> GameState.GameState -> [Maybe String]
faceNames oids gs = fmap (\oid -> fmap (Text.unpack . CardName.unwrap . Face.name) (Game.faceOf oid gs)) oids
