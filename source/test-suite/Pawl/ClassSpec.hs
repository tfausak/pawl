{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 716's Class cards, which need no engine subsystem of their own. A
-- class level bar is a keyword ability (CR 716.2) whose meaning rule 716.2a spells
-- out in full, and both halves of that sentence are vocabulary the card model
-- already had: the activated half is an ordinary activated ability with an
-- ActivationRestriction.SorcerySpeed and an ActivatedAbility.condition, and the
-- static half is an ordinary StaticAbility with a CR 604.2 clause. What this unit
-- added is the mark those two clauses read -- Object.classLevel, written by
-- Effect.SetClassLevel and read by Quantity.ClassLevel.
--
-- So what this file exercises is that mark's lifecycle: Pawl.Engine.Resolve's
-- SetClassLevel arm writes it, Pawl.Engine.Quantity's ClassLevel arm reads it back
-- through Pawl.Engine.Filter's view, Pawl.Engine.Projection.abilitiesFromCharacteristics
-- gates the next bar on it, and Pawl.Engine.Projection.gatherStatic gates the
-- section's continuous effect on it.
--
-- Paladin Class, AFR 29, is the whole card pool for this file, and it was picked
-- for what its level-2 section is: "Creatures you control get +1/+1", a plain
-- layer 7c modification, so nothing but the level gate is under test. Its two
-- other text-box sections are transcribed SHORT, and both omissions leave pawl's
-- card strictly WEAKER than printed rather than stronger:
--
--   * "Spells your opponents cast during your turn cost {1} more to cast" (CR
--     716.3's top section) is omitted. Pawl.Types.PlayerStaticAbility carries a
--     scope and an effect and no condition, so "during your turn" is unwritable
--     (gap #1945).
--   * The level-3 section's "Whenever you attack, ..." is omitted; the BAR is
--     kept, since CR 716.2a's activated half is what the level-3 gate below is
--     about. The section's own text needs a count that excludes the slot it is
--     aimed at (gap #1946); the shape that would carry it, a level-gated grant of
--     a quoted triggered ability, landed with #1943 and is unused here.
--
-- CR 716.1 is a frame rule with no rules meaning -- the striated text box and the
-- sideways type line -- so nothing here asserts about the layout, the same reading
-- Pawl.RoomSpec's CR 709 note takes.
--
-- What is NOT proven here, because Paladin Class cannot reach it:
--
--   * CR 716.2b's "a Class retains its level even if it stops being a Class" and
--     "levels are not a copiable characteristic". The first needs an effect that
--     removes a subtype from an enchantment; the second falls out by construction,
--     a copy effect's payload being a ProjectedCharacteristics and never an Object
--     (see Object.classLevel's own note). Neither has a producer (gap #1947).
--   * CR 716.2c's "to gain a Class level", which only Sorcerer Class prints (gap
--     #1948), and CR 716's "when this Class becomes level N" triggers (gap
--     #1944).
module Pawl.ClassSpec where

import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Class" $ do
  sectionSpec s registry
  ladderSpec s registry

-- alice's board: the Class and one Goblin Piker on the battlefield, twelve
-- untapped Plains, in her own precombat main phase with priority. Twelve is more
-- than both bars together cost ({2}{W} then {4}{W}), so no assertion below can
-- turn on affordability -- which matters for the ladder case, whose whole point
-- is that a bar it CAN pay for is still not offered.
--
-- Goblin Piker is the creature because it is vanilla and 2/1: the two axes carry
-- different numbers, so a +1/+1 that landed on only one of them could not read as
-- the whole modification.
--
-- Both permanents are placed rather than cast: CR 716 says nothing about how a
-- Class arrives, and Paladin Class has no entry trigger.
board :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
board paladinClass plains piker =
  let (classId, withClass) = S.addCreature paladinClass S.alice (S.landsInPlay plains 12)
      (pikerId, withPiker) = S.addCreature piker S.alice withClass
   in ( classId,
        pikerId,
        withPiker
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- CR 716.2b's mark itself, which no characteristic reports. Nothing is a Class
-- that has never been levelled, which CR 716.2d then reads as level 1 -- the
-- default lives at Pawl.Engine.Quantity's read and deliberately not here.
levelOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe ClassLevel.ClassLevel
levelOf oid gs = Game.lookupObject oid gs >>= Object.classLevel

-- The level bars alice may activate on this permanent RIGHT NOW, off the same
-- enumeration a player is offered. Counts activations of one source, so the
-- Plains' mana abilities and the Piker cannot be mistaken for one.
barsOffered :: ObjectId.ObjectId -> GameState.GameState -> Int
barsOffered oid gs = length [() | Action.Type.Activate o _ <- Action.legalActions S.alice gs, o == oid]

-- Activate the first bar the enumeration offers and resolve it. Stack.resolveTop
-- rather than the priority loop: the narrowest path that shows the write, with no
-- settle in between that could sweep something.
gainLevel :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
gainLevel oid gs = case Activate.abilitiesFor oid gs of
  [] -> gs
  ability : _ ->
    let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
     in S.runPure S.identityAnswer activated Stack.resolveTop

-- CR 716.2a's static half: "As long as this Class is level N or greater, it has
-- [abilities]."
sectionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
sectionSpec s registry = Spec.describe s "Level bar section" $ do
  -- The two readings this case must tell apart: a level-gated section that is off
  -- until the level is gained, and one that is simply always on. The Piker's
  -- printed 2/1 is what the second would have destroyed.
  Spec.it s "CR 716.2d a Class with no level reads as level 1, so its level-2 section is off" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, pikerId, gs) = board paladinClass plains piker
    Spec.assertEqWith s "the Piker is its printed 2/1" (S.powerToughnessOf pikerId gs) (Just (2, 1))
    Spec.assertEqWith s "CR 716.2b: no level designation has been written" (levelOf classId gs) Nothing
  Spec.it s "CR 716.2a the level-2 section functions once the Class is level 2" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, pikerId, gs) = board paladinClass plains piker
        levelled = gainLevel classId gs
    Spec.assertEqWith s "the Piker is 3/2 while the Class is level 2" (S.powerToughnessOf pikerId levelled) (Just (3, 2))
    Spec.assertEqWith s "CR 716.2a: the level BECAME 2" (levelOf classId levelled) (Just (ClassLevel.MkClassLevel 2))

-- CR 716.2a's activated half: "[Cost]: This Class's level becomes N. Activate only
-- if this Class is level N-1 and only as a sorcery."
ladderSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
ladderSpec s registry = Spec.describe s "Level bar activation" $ do
  -- One bar at a time, in order, and then nothing. The falsifier for a missing
  -- "only if this Class is level N-1" is the last entry: with no such clause the
  -- level-2 bar stays offered forever, and the third activation would put the
  -- level back to 2.
  Spec.it s "CR 716.2a a bar is activatable only from level N-1" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, _, gs) = board paladinClass plains piker
        climb n = iterate (gainLevel classId) gs !! n
    Spec.assertEqWith
      s
      "the level after 0, 1, 2 and 3 activations"
      (fmap (levelOf classId . climb) [0, 1, 2, 3])
      [Nothing, Just (ClassLevel.MkClassLevel 2), Just (ClassLevel.MkClassLevel 3), Just (ClassLevel.MkClassLevel 3)]
    Spec.assertEqWith
      s
      "the bars offered at levels 1, 2 and 3"
      (fmap (barsOffered classId . climb) [0, 1, 2])
      [1, 1, 0]
  -- Two boards differing in exactly one thing: whose turn it is. Both hold the
  -- same twelve untapped Plains, so the bar is affordable on each, and CR 307.5's
  -- "during a main phase of their turn" is the only thing that moves.
  Spec.it s "CR 716.2a a level bar is activatable only as a sorcery" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, _, gs) = board paladinClass plains piker
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertEqWith s "offered on alice's own main phase" (barsOffered classId gs) 1
    Spec.assertEqWith s "not offered on bob's turn" (barsOffered classId bobsTurn) 0
