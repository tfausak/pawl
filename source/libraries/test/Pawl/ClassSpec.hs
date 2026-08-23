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
-- layer 7c modification, so nothing but the level gate is under test. Its TOP
-- section -- "Spells your opponents cast during your turn cost {1} more to cast",
-- which CR 716.3 makes an ability the Class has at all times -- is transcribed
-- too, and topSectionSpec below is what proves it. Its remaining section is
-- transcribed SHORT, and the omission leaves pawl's card strictly WEAKER than
-- printed rather than stronger:
--
--   * The level-3 section's "Whenever you attack, ..." is omitted; the BAR is
--     kept, since CR 716.2a's activated half is what the level-3 gate below is
--     about. The section's own text needs a count that excludes the slot it is
--     aimed at (gap #1946); the shape that would carry it, a level-gated grant of
--     a quoted triggered ability, landed with #1943 and is unused here.
--
-- CR 716.1 is a frame rule with no rules meaning -- the striated text box and the
-- sideways type line -- so nothing here asserts about the layout, and
-- data/cards/paladin-class.json states none.
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
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Class" $ do
  topSectionSpec s registry
  sectionSpec s registry
  ladderSpec s registry

-- CR 716.3: the text above the first class level bar is an ability the Class has
-- at ALL times -- no bar precedes it, so no level gates it. Paladin Class prints
-- "Spells your opponents cast during your turn cost {1} more to cast", and the
-- clause under test is the "during your turn": a CR 604.2 condition on the
-- PLAYER-facing static carrier, reading CR 102.1's active player.
--
-- alice controls the Class and nothing else; bob and alice each hold a Lightning
-- Bolt ({R}). No lands and no mana: Cost.total answers about a cost rather than
-- about paying it, so affordability cannot enter any assertion here.
--
-- The two boards below differ in EXACTLY one thing, GameState.activePlayer, and
-- the Class is never levelled -- CR 716.3's section is not a level bar's, so a
-- level would be a second moving part with nothing to prove.
taxBoard :: Printing.Printing -> Printing.Printing -> PlayerId.PlayerId -> (ObjectId.ObjectId, GameState.GameState)
taxBoard paladinClass lightningBolt active =
  let (_, withClass) = S.addCreature paladinClass S.alice (Setup.emptyGame S.bothPlayers)
      (bolt, withBolt) = S.addHandCard lightningBolt S.bob withClass
   in ( bolt,
        withBolt
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = active,
            GameState.priority = Just active
          }
      )

-- What `pid` would pay in total to cast the object in their hand whose printed
-- mana cost is `printed`. PlayerEffectSpec's totalManaCost, duplicated rather
-- than hoisted: Pawl.Support rebuilds every spec in the tree.
totalManaCost :: PlayerId.PlayerId -> ObjectId.ObjectId -> ManaCost.ManaCost -> GameState.GameState -> Maybe ManaCost.ManaCost
totalManaCost pid oid printed gs = Cost.Type.mana (Cost.total pid oid (Cost.Type.MkCost (Just printed) []) gs)

red :: ManaSymbol.ManaSymbol
red = ManaSymbol.OfType (ManaType.Colored Color.Red)

topSectionSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
topSectionSpec s registry = Spec.describe s "Top text box section" $ do
  -- The gameplay-level assertion first, and it is the one the whole unit exists
  -- for: an opponent's spell is taxed on alice's turn and NOT on bob's own turn.
  -- A Class whose clause was dropped taxes on both, and a clause read from the
  -- TAXED player's perspective rather than the Class controller's taxes on
  -- neither -- so the pair discriminates both wrong readings, which no single
  -- board can.
  Spec.it s "CR 716.3 / CR 102.1 the tax applies only during the Class controller's turn" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (bolt, alicesTurn) = taxBoard paladinClass lightningBolt S.alice
        (bobsBolt, bobsTurn) = taxBoard paladinClass lightningBolt S.bob
    Spec.assertEqWith
      s
      "bob's {R} Bolt costs {1}{R} during alice's turn"
      (totalManaCost S.bob bolt (ManaCost.MkManaCost [red]) alicesTurn)
      (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, red]))
    Spec.assertEqWith
      s
      "the same Bolt costs {R} during bob's own turn"
      (totalManaCost S.bob bobsBolt (ManaCost.MkManaCost [red]) bobsTurn)
      (Just (ManaCost.MkManaCost [red]))
  -- The scope half, which the case above cannot see: "your opponents" and "each
  -- player" agree on every board where only an opponent casts. alice casting on
  -- her own turn -- the one turn the condition is true -- is where they differ.
  Spec.it s "CR 716.3 the Class controller's own spell is untaxed on her own turn" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let (_, withClass) = S.addCreature paladinClass S.alice (Setup.emptyGame S.bothPlayers)
        (alicesBolt, gs) = S.addHandCard lightningBolt S.alice withClass
        alicesTurn = gs {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice, GameState.priority = Just S.alice}
    Spec.assertEqWith
      s
      "alice's own {R} Bolt stays {R}"
      (totalManaCost S.alice alicesBolt (ManaCost.MkManaCost [red]) alicesTurn)
      (Just (ManaCost.MkManaCost [red]))

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
  -- "it must be during the main phase of their turn" is the only thing that moves.
  Spec.it s "CR 716.2a a level bar is activatable only as a sorcery" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, _, gs) = board paladinClass plains piker
        bobsTurn = gs {GameState.activePlayer = S.bob}
    Spec.assertEqWith s "offered on alice's own main phase" (barsOffered classId gs) 1
    Spec.assertEqWith s "not offered on bob's turn" (barsOffered classId bobsTurn) 0
