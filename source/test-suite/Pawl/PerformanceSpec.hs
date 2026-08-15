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
-- TWO FIXTURES, because CR 605.1a's mana-ability test cuts the enumeration's
-- gate chain in half and one printing can only measure one side of it. Llanowar
-- Elves stops at that test and holds the RATIO guard; Prodigal Sorcerer runs
-- past it into the target and cost gates and holds an ABSOLUTE per-permanent
-- ceiling. See boardOf and sorcererCeilingBytesPerPermanent for why the second
-- one cannot be a ratio: after #716 and #1073 that path takes a constant number
-- of whole-board projections, base target pools and mana-source sweeps, but it
-- still FILTERS a whole-board candidate set per ability and so is still
-- quadratic overall (#1448).
--
-- Only the action enumeration is measured. The other paths the priority loop
-- reaches every pass -- Combat.legalAttackers, Combat.legalBlockers,
-- Mana.manaSources, Cast.castableSpells -- have no bound here (#717).
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
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Zone as Zone

-- The two board sizes every measurement here is taken at. Their RATIO is what
-- the growth guard reads: 4x the board costs about 4x a linear enumeration and
-- about 16x one that projects the board per object. The larger of the two is
-- twice the benchmark's end-of-game boards (115-120 permanents), which is big
-- enough to separate those two readings and small enough that the whole module
-- runs in hundredths of a second.
smallBoard, largeBoard :: Int
smallBoard = 64
largeBoard = 256

-- `n` copies of a printing on the battlefield under alice, in her precombat
-- main phase.
--
-- Never a vanilla creature: the printing has to carry an activated ability, or
-- the loop reads an empty list of PROJECTED activated abilities
-- (Activate.abilitiesForGiven, the reader #315 made expensive) and the
-- per-object gates never run at all. The two printings this module uses stop at
-- two different depths of that gate chain, which is why it takes both:
--
--   * LLANOWAR ELVES stops where CR 605.1a's mana-ability test does, before
--     Cost.canPaySomeCompletion and Target.fillableModes. That short path is
--     the one the ratio guard below reads, and it is linear. CR 605.3a's
--     priority window offers the Elf's ability anyway (#1123), through
--     Mana.manaSourcesGiven -- ONE sweep for the whole enumeration rather than
--     a per-object gate, so the path this fixture measures is unchanged.
--   * PRODIGAL SORCERER's "{T}: deals 1 damage to any target" is not a mana
--     ability (CR 605.1a: it requires a target), so its enumeration runs every
--     remaining conjunct, including the two that ask about OTHER objects. That
--     path is the one the absolute Sorcerer ceiling below reads.
--
-- The hand, the graveyard and exile are all empty, so Cast.castableSpells and
-- playableLands have no candidates to walk here -- both scan all three zones --
-- and the battlefield loop is the only thing that varies with `n`. The zone
-- permissions playableLands consults fold over the battlefield, but
-- Action.legalActions already folds it once for CR 305.2's allowance, so that is
-- a constant factor on a loop the board had anyway.
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

-- `n` of EACH printing on the battlefield under alice, in her precombat main
-- phase. boardOf's mixed twin, for the assertion that pins the enumeration's
-- ANSWER rather than its cost -- see "the enumeration answers what the
-- unhoisted wrappers answer" for why one printing is not enough there.
mixedBoard :: [Printing.Printing] -> Int -> GameState.GameState
mixedBoard printings n =
  let addOne gs printing = snd (S.addCreature printing S.alice gs)
      board = List.foldl' addOne (Setup.emptyGame S.bothPlayers) (concat (replicate n printings))
   in board {GameState.phase = Phase.PrecombatMain}

-- Activate.activatableGiven fed everything Action.legalActions hoists for it.
-- One expression, so the spec below reads as a differential rather than as a
-- pile of arguments.
threadedGate :: PlayerId.PlayerId -> ObjectId.ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState.GameState -> Bool
threadedGate pid oid ability gs =
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
   in Activate.activatableGiven grants pcs (Target.poolsGiven pcs gs) (Cost.activationManaSourcesGiven grants pcs pid gs) pid oid ability gs

-- Action.legalActions' two ACTIVATION lists rebuilt out of the plain per-call
-- wrappers, which hoist nothing: Activate.activatable projects each object for
-- itself and builds a fresh base pool and a fresh mana-source sweep per ability,
-- and Mana.manaSources takes its own board. Same zones in the same order as the
-- enumeration, so the two lists are comparable element for element and a hoist
-- that reordered the menu would show up here.
--
-- Only those two, because they are the only ones #716 and #1073 touched. The
-- other seven kinds of action in the menu are enumerated identically either way,
-- so the spec compares this against the enumeration's activation slice.
referenceActivations :: PlayerId.PlayerId -> GameState.GameState -> [Action.Type.Action]
referenceActivations pid gs =
  let forObject oid = fmap (Action.Type.Activate oid) (filter (\ab -> Activate.activatable pid oid ab gs) (Activate.abilitiesFor oid gs))
      zones = Projection.controls pid gs <> Game.zoneMembers Zone.Hand pid gs <> Game.zoneMembers Zone.Graveyard pid gs
   in concatMap forObject zones <> fmap Action.Type.ActivateManaAbility (Mana.manaSources Cost.manaActivations pid gs)

-- The enumeration's activation slice. EXHAUSTIVE over Action, so a new kind of
-- action has to be classified here rather than silently dropping out of the
-- comparison above.
activationsIn :: [Action.Type.Action] -> [Action.Type.Action]
activationsIn =
  let isActivation action = case action of
        Action.Type.Activate _ _ -> True
        Action.Type.ActivateManaAbility _ -> True
        Action.Type.Pass -> False
        Action.Type.Play _ _ -> False
        Action.Type.Cast {} -> False
        Action.Type.TurnFaceUp _ -> False
        Action.Type.Unlock _ _ -> False
        Action.Type.DiscardFromHand _ -> False
        Action.Type.Plot _ -> False
        Action.Type.Foretell _ -> False
        Action.Type.Ignore _ -> False
   in filter isActivation

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
-- Measured at 9,188 bytes per permanent on GHC 9.14.1, so this carries ~1.3x
-- headroom: enough to absorb a compiler bump, a change of architecture (this
-- was measured on aarch64 and CI runs the suite on x86_64), or a feature
-- landing in the enumeration -- and still tight enough to fail on a doubling.
-- It read 7,515 before CR 605.3a's priority window (#1123) added an action per
-- source to the list this walk builds.
-- To REGENERATE it, run this test and read the observed figure out of the
-- failure message; if the increase is understood and wanted, raise this
-- constant in the commit that causes it.
ceilingBytesPerPermanent :: Integer
ceilingBytesPerPermanent = 12000

-- The same committed ceiling for the OTHER fixture -- the one whose ability is
-- not a mana ability, so the enumeration runs the two conjuncts that ask about
-- other objects (CR 700.2a's fillable-mode test and CR 118.3's payability
-- test). Those two are the ones #716 threaded the enumeration's own board into
-- and #1073 hoisted the rest of the way, and this is what holds both in place.
--
-- AN ABSOLUTE CEILING AND DELIBERATELY NOT A RATIO, because this board is STILL
-- QUADRATIC and the ratio would say so without saying anything about the
-- hoists. What is left per permanent is the pool FILTER rather than the pool:
-- Target.legalRecipientsGiven runs `targetable` over every candidate of the
-- shared base pool for every slot of every ability, and PlayerEffect.applying
-- walks the battlefield once per player candidate (#1448, #435). Measured at 11.1x for
-- a 4x board (2,293,288 bytes at 64 permanents, 25,400,600 at 256), against
-- 16.5x before the hoists -- so a growth bound here would fail at 8x and assert
-- nothing at 14x.
--
-- THE BRACKET this number sits in, all measured at largeBoard on GHC 9.14.1 /
-- aarch64:
--
--   * 99,221 as it stands.
--   * 571,352 with the base pools taken away again (build them per slot in
--     Target.basePoolGiven rather than reading Action.legalActions' Pools).
--   * 813,080 with the mana-source sweep taken away too, which is the pre-#1073
--     tree.
--
-- So this carries ~1.3x headroom over the reading -- enough for a compiler bump
-- or the x86_64 CI runner -- and catches the pool regression 5.7x over. It does
-- NOT catch losing the MANA-SOURCE hoist alone, which the two figures above put
-- at about 1.4x: no ceiling could, without being tight enough to fail on a
-- differently sized allocation. What holds that one is that it is a single
-- `let` in Action.legalActions read by both the cost gate and CR 605.3a's
-- offer -- losing it means deleting a shared binding rather than drifting into
-- it. Saying so is better than implying a coverage this number does not have.
--
-- REGENERATED exactly as ceilingBytesPerPermanent above is: run this test and
-- read the observed figure out of the failure message.
sorcererCeilingBytesPerPermanent :: Integer
sorcererCeilingBytesPerPermanent = 130000

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
    -- CR 605.3a: one ActivateManaAbility per Elf, plus Pass. No Activate
    -- rides beside it -- CR 605.3b keeps a mana ability off the stack, so
    -- Activate.activatableGiven still refuses it at CR 605.1a and the gate
    -- chain the ratio guard measures is still the SHORT one.
    Spec.assertEqWith s "and the enumeration answers one mana activation per permanent at both sizes" (fmap (enumerationOver elves) [smallBoard, largeBoard]) [smallBoard + 1, largeBoard + 1]

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

  -- The discriminating fixture check for the Sorcerer board, and the one the
  -- measurement below is worthless without.
  Spec.it s "the Prodigal Sorcerer fixture reaches every conjunct of the enumeration" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let board = boardOf sorcerer smallBoard
        oids = Set.toList (GameState.battlefield board)
    Spec.assertEqWith s "the board holds one permanent per requested size" (length oids) smallBoard
    Spec.assertEqWith s "each one offers exactly one activated ability to enumerate" (fmap (\oid -> length (Activate.abilitiesFor oid board)) oids) (replicate smallBoard 1)
    -- CR 602.2: one Activate action per permanent, plus Pass. THE load-bearing
    -- assertion here: an offered activation means CR 605.1a's mana-ability
    -- test, the CR 302.6 sickness gate, CR 700.2a's fillable-mode test AND CR
    -- 118.3's payability test all ran and all said yes. If any conjunct
    -- refused, this reads 1 -- exactly what the Llanowar Elves fixture reads --
    -- and every allocation figure taken over this board is vacuous.
    Spec.assertEqWith s "and the enumeration answers one activation per permanent at both sizes" (fmap (enumerationOver sorcerer) [smallBoard, largeBoard]) [smallBoard + 1, largeBoard + 1]

  -- Threading the enumeration's board into those last two conjuncts (#716) must
  -- change no ANSWER, only what the answer costs. This is the direct check that
  -- it did not.
  --
  -- A genuine differential rather than one expression under two names:
  -- Activate.activatable passes Map.empty for the projected board, so the plain
  -- side really does project each object for itself through
  -- Projection.projectGiven's per-object fallback while the threaded side reads
  -- the pre-projected board. It is still a REGRESSION FENCE and not a proof,
  -- since both sides read the same GameState -- the proof is the snapshot
  -- argument at Projection.projectGiven.
  Spec.it s "the threaded board answers what the unthreaded one answers" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    let board = boardOf sorcerer 8
        oids = Set.toList (GameState.battlefield board)
        viaPlain oid = filter (\ab -> Activate.activatable S.alice oid ab board) (Activate.abilitiesFor oid board)
        viaThreaded oid = filter (\ab -> threadedGate S.alice oid ab board) (Activate.abilitiesFor oid board)
    Spec.assertEqWith s "the plain gate offers something to differ about" (fmap (length . viaPlain) oids) (replicate 8 1)
    Spec.assertEqWith s "and the threaded gate agrees with it object for object" (fmap viaThreaded oids) (fmap viaPlain oids)

  -- THE WHOLE MENU, not one gate: the same list Action.legalActions answers,
  -- rebuilt out of the plain per-call wrappers that hoist nothing. #1073 hoisted
  -- two more whole-board structures out of the per-ability loop -- the base
  -- target pools and the mana-source sweep -- and a hoist that drops or reorders
  -- a legal action is a rules bug rather than a slow one, so this pins the
  -- ANSWER rather than the cost.
  --
  -- A MIXED board, and that is what makes it discriminate: a board of one
  -- printing cannot tell "every permanent answered" from "the first permanent's
  -- answer was reused". These four differ in every conjunct the hoists reach --
  -- a targeting activation (Prodigal Sorcerer), a mana ability with no target
  -- (Llanowar Elves), a land whose mana ability costs no {T} of a creature
  -- (Mountain), and a creature with no activated ability at all (Hill Giant),
  -- so the reference and the enumeration each answer a different action count
  -- per permanent.
  Spec.it s "the enumeration answers what the unhoisted wrappers answer" $ do
    printings <- traverse (S.printingOf s registry) ["Prodigal Sorcerer", "Llanowar Elves", "Mountain", "Hill Giant"]
    let board = mixedBoard printings 6
        reference = referenceActivations S.alice board
    -- 6 Sorcerer activations + 6 Elf and 6 Mountain mana activations, and
    -- nothing from the Bears: three different answers on one board, so a hoist
    -- that reused one permanent's answer cannot pass this.
    Spec.assertEqWith s "the reference offers three different per-permanent answers to differ about" (length reference) 18
    Spec.assertEqWith s "and the hoisted enumeration answers it action for action" (activationsIn (Action.legalActions S.alice board)) reference

  Spec.it s "enumerating a board of non-mana abilities stays within its committed allocation ceiling" $ do
    sorcerer <- S.printingOf s registry "Prodigal Sorcerer"
    _ <- allocationsOf (enumerationOver sorcerer) 4
    large <- allocationsOf (enumerationOver sorcerer) largeBoard
    let perPermanent = large `div` toInteger largeBoard
    Spec.assertLeWith s ("observed " <> show perPermanent <> " bytes per permanent (" <> show large <> " bytes over " <> show largeBoard <> " permanents)") perPermanent sorcererCeilingBytesPerPermanent
