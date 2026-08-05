-- Covers the SCALING of Pawl.Engine.Action.legalActions, and through it
-- Pawl.Engine.Activate's enumeration path and Pawl.Engine.Projection's threaded
-- readers. Nothing here is about the rules: every assertion is about what the
-- enumeration COSTS, which nothing else in the suite measures at all (#578).
--
-- The property under guard is the one #200, #315 and #316 all turned on:
-- enumerating one player's legal actions performs a CONSTANT number of
-- whole-board projections, not one per object. A projection is itself a walk of
-- the battlefield, so one per object makes the enumeration quadratic in board
-- size -- which is what happened when #315 gave Activate.abilitiesFor a zone
-- case, GHC stopped sharing the repeated `project srcId gs`, and the suite went
-- from 29s to 56s with nothing failing.
--
-- Measured in BYTES ALLOCATED rather than in seconds. Allocation is a
-- deterministic function of the code and its input: the same build measuring
-- the same board allocates the same amount every run, on a loaded shared runner
-- as on an idle laptop. Milliseconds are not, and a check that fails
-- intermittently is worse than no check at all, because it teaches people to
-- ignore it.
module Pawl.PerformanceSpec where

import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Set as Set
import qualified GHC.Conc as Conc
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing

-- The two board sizes every measurement here is taken at. Their RATIO is what
-- the growth guard reads: 4x the board costs about 4x a linear enumeration and
-- about 16x one that projects the board per object. The larger of the two is
-- twice the benchmark's end-of-game boards (115-120 permanents), which is big
-- enough to separate those two readings and small enough that the whole module
-- runs in hundredths of a second.
smallBoard, largeBoard :: Int
smallBoard = 64
largeBoard = 256

-- `n` Llanowar Elves on the battlefield under alice, in her precombat main
-- phase.
--
-- Llanowar Elves and not a vanilla creature because its printed "{T}: Add {G}"
-- makes the enumeration do the work under guard: the loop reads every
-- permanent's PROJECTED activated abilities (Activate.abilitiesForGiven, the
-- reader #315 made expensive) and classifies each one. A vanilla body would
-- leave that list empty and the per-object gates would never run at all.
--
-- It stops where CR 605.1a's mana-ability test does, which is before
-- Cost.canPay and Target.fillableModes. That is deliberate: those two ask about
-- OTHER objects and take a whole-board sweep apiece, so a board whose abilities
-- reach them is quadratic today and NOT guarded here -- swapping this fixture
-- for Prodigal Sorcerer reads 15.6x rather than 3.8x (#716). This module
-- measures the projection loop the enumeration itself runs, so the fixture
-- keeps those two sweeps out of the reading rather than widening the bound to
-- admit them.
--
-- The hand is empty, so Cast.castableSpells and playableLands are constant work
-- here and the battlefield loop is the only thing that varies with `n`.
boardOf :: Printing.Printing -> Int -> GameState.GameState
boardOf printing n =
  let addOne gs _ = snd (S.addCreature printing S.alice gs)
      board = List.foldl' addOne (Setup.emptyGame S.bothPlayers) [1 .. n]
   in board {GameState.phase = Phase.PrecombatMain}

-- One whole enumeration over a board of `n` of a printing, forced.
--
-- NOINLINE, and taking the SIZE as its argument rather than a prebuilt state,
-- so that two measurements at two sizes are two different expressions. GHC may
-- share two occurrences of one pure expression, and a shared second measurement
-- would read as no allocation at all and pass every bound below vacuously.
--
-- `length` forces the list's spine, which is the whole enumeration: the
-- per-object filter cannot say whether an object contributes an action without
-- running every gate on it.
enumerationOver :: Printing.Printing -> Int -> Int
enumerationOver printing n = length (Action.legalActions S.alice (boardOf printing n))
{-# NOINLINE enumerationOver #-}

-- Bytes the calling thread allocates while forcing `f n`.
--
-- GHC.Conc's allocation counter is PER HASKELL THREAD and counts DOWN, so the
-- drop across the call is this thread's own allocation and no test running
-- beside it can perturb the reading. It needs no RTS option, no profiling build
-- and no -T. Its granularity is the nursery block, which is nothing against the
-- megabyte the large board allocates.
allocationsOf :: (Int -> Int) -> Int -> IO Integer
allocationsOf f n = do
  before <- Conc.getAllocationCounter
  _ <- Exception.evaluate (f n)
  after <- Conc.getAllocationCounter
  pure (toInteger before - toInteger after)

-- How much more the 4x board may allocate before this is called a regression.
--
-- Measured at 3.8x on GHC 9.14.1 -- just under the 4x a linear enumeration
-- predicts, since part of the cost is fixed. The number to beat is the 16x one
-- projection per object would cost, and this bound sits at the geometric middle
-- of the two: 2.1x the headroom the current reading needs, and half of the
-- smallest quadratic reading it has to catch.
--
-- The ratio is what makes this stable without a committed baseline. A change
-- that makes the enumeration twice as expensive per object moves both readings
-- and leaves the ratio alone; only a change to the SHAPE of the loop moves it.
-- The headroom is there for an O(n log n) step -- a sort of the battlefield
-- would read as 5.3x -- rather than for measurement noise, of which there is
-- none.
growthBound :: Integer
growthBound = 8

-- The committed allocation ceiling, in BYTES PER PERMANENT at largeBoard. This
-- is the half the growth bound above cannot see: a change that doubles what
-- every object costs without changing the shape of the loop keeps the ratio at
-- 4x, and only an absolute figure catches it.
--
-- Measured at 5,448 bytes per permanent on GHC 9.14.1, so this carries ~2.2x
-- headroom: enough to absorb a compiler bump, a change of architecture (this
-- was measured on aarch64 and CI runs the suite on x86_64), or a feature
-- landing in the enumeration -- and still tight enough to fail on a doubling.
-- To REGENERATE it, run this test and read the observed figure out of the
-- failure message; if the increase is understood and wanted, raise this
-- constant in the commit that causes it.
ceilingBytesPerPermanent :: Integer
ceilingBytesPerPermanent = 12000

spec :: (Monad n) => Spec.Spec IO n -> Registry.Registry IO -> n ()
spec s registry = Spec.describe s "performance" $ do
  -- The discriminating fixture check. Everything else here asserts a number
  -- rather than a behavior, so it would go on passing if the board silently
  -- stopped holding permanents or the enumeration stopped looking at them.
  Spec.it s "the fixture is a real board the enumeration really walks" $ do
    elves <- S.printingOf s registry "Llanowar Elves"
    let board = boardOf elves smallBoard
        oids = Set.toList (GameState.battlefield board)
    Spec.assertEqWith s "the board holds one permanent per requested size" (length oids) smallBoard
    Spec.assertEqWith s "each one offers exactly one activated ability to enumerate" (fmap (\oid -> length (Activate.abilitiesFor oid board)) oids) (replicate smallBoard 1)
    -- CR 605.3b: an activated mana ability doesn't go on the stack, so it is
    -- not an offered action and alice may only pass. That the ANSWER does not
    -- grow with the board is what makes the readings below a measure of the
    -- walk rather than of the list it builds.
    Spec.assertEqWith s "and the enumeration answers Pass alone at both sizes" (fmap (enumerationOver elves) [smallBoard, largeBoard]) [1, 1]

  Spec.it s "enumerating legal actions costs one board projection, not one per object" $ do
    elves <- S.printingOf s registry "Llanowar Elves"
    -- A throwaway measurement first, at neither size, so that whatever the
    -- first enumeration in this process forces once -- a CAF in the projection,
    -- the card's parsed face -- is not charged to the small board and read as
    -- growth that is not there.
    _ <- allocationsOf (enumerationOver elves) 4
    small <- allocationsOf (enumerationOver elves) smallBoard
    large <- allocationsOf (enumerationOver elves) largeBoard
    let sizeRatio = toInteger largeBoard `div` toInteger smallBoard
        observed = "a " <> show largeBoard <> "-permanent board allocated " <> show large <> " bytes against the " <> show smallBoard <> "-permanent board's " <> show small
    Spec.assertGtWith s "the counter measured something" small 0
    Spec.assertLeWith s (observed <> "; " <> show sizeRatio <> "x the board may cost at most " <> show growthBound <> "x, and one projection per object costs about " <> show (sizeRatio * sizeRatio) <> "x") large (growthBound * small)

  Spec.it s "enumerating legal actions stays within its committed allocation ceiling" $ do
    elves <- S.printingOf s registry "Llanowar Elves"
    _ <- allocationsOf (enumerationOver elves) 4
    large <- allocationsOf (enumerationOver elves) largeBoard
    let perPermanent = large `div` toInteger largeBoard
    Spec.assertLeWith s ("observed " <> show perPermanent <> " bytes per permanent (" <> show large <> " bytes over " <> show largeBoard <> " permanents)") perPermanent ceilingBytesPerPermanent
