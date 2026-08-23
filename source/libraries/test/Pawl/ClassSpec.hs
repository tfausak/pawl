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
-- Paladin Class, AFR 29, is the card under test throughout, and it was picked
-- for what its level-2 section is: "Creatures you control get +1/+1", a plain
-- layer 7c modification, so nothing but the level gate is under test. Its TOP
-- section -- "Spells your opponents cast during your turn cost {1} more to cast",
-- which CR 716.3 makes an ability the Class has at all times -- is transcribed
-- too, and topSectionSpec below is what proves it. Its LEVEL-3 section --
-- "Whenever you attack, until end of turn, target attacking creature gets +1/+1
-- for each other attacking creature and gains double strike" -- is transcribed
-- as well, and levelThreeSpec below is what proves it. Nothing on the card is
-- omitted.
--
-- CR 716.1 is a frame rule with no rules meaning -- the striated text box and the
-- sideways type line -- so nothing here asserts about the layout, and
-- data/cards/paladin-class.json states none.
--
-- What is NOT proven here, because Paladin Class cannot reach it:
--
--   * CR 716.2c's "to gain a Class level", which only Sorcerer Class prints (gap
--     #1948), and CR 716's "when this Class becomes level N" triggers (gap
--     #1944).
module Pawl.ClassSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Class" $ do
  topSectionSpec s registry
  sectionSpec s registry
  levelThreeSpec s registry
  ladderSpec s registry
  designationSpec s registry

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

-- CR 716.2a's static half at the LAST section, which is where Paladin Class's
-- ladder ends: "Whenever you attack, until end of turn, target attacking
-- creature gets +1/+1 for each other attacking creature and gains double
-- strike."
--
-- Three rules meet here, each with its own falsifier on the boards below:
--
--   * CR 716.2a. The section is a static ability gated on level 3 that grants
--     the Class a quoted TRIGGERED ability (CR 613.1f). The level-2 board is the
--     negative, and differs from the positive in exactly one field.
--   * CR 508.3d. "Whenever you attack" asks about the attacking PLAYER and
--     triggers once per declaration, so four attackers make one trigger and one
--     pump -- not four.
--   * "for each OTHER attacking creature", which excludes the creature the pump
--     is aimed at.
--
-- FOUR attackers is what makes that exclusion visible: the right count is 3 and
-- a count that forgot the exclusion is 4, so the target reads 6/5 rather than
-- 7/6. One attacker would put the two readings at 0 and 1, which differ too --
-- but four also keeps the pump clear of the level-2 section's own +1/+1, which
-- is still on at level 3 (its gate is "2 or greater") and lands on all four.
--
-- Goblin Piker on every seat: vanilla, so nothing else can move a number, and
-- 2/1 rather than square, so a modification landing on one axis only could not
-- read as both. bob defends with NOTHING, so there is no blocker to deal the
-- attackers damage and CR 704.5g cannot destroy the creature the assertions read
-- before they read it.

-- alice's four Settled Pikers and her Class, at the level given; bob empty.
--
-- The level is WRITTEN rather than climbed. Pawl.Support.combatBoardOf opens in
-- the declare attackers step, where CR 307.5 forbids the bar's sorcery-speed
-- activation, and ladderSpec below is what proves the climb; here the level is a
-- precondition, and each case asserts it on the board.
attackBoard :: Printing.Printing -> Printing.Printing -> Natural.Natural -> ([ObjectId.ObjectId], ObjectId.ObjectId, GameState.GameState)
attackBoard paladinClass piker level =
  let (gs, pikers, _) = S.combatBoardOf (replicate 4 piker) []
      (classId, withClass) = S.addCreature paladinClass S.alice gs
   in (pikers, classId, atLevel classId level withClass)

-- CR 716.2b's designation, written straight onto the permanent.
atLevel :: ObjectId.ObjectId -> Natural.Natural -> GameState.GameState -> GameState.GameState
atLevel oid level gs =
  gs
    { GameState.objects =
        Map.adjust (\o -> o {Object.classLevel = Just (ClassLevel.MkClassLevel level)}) oid (GameState.objects gs)
    }

levelThreeSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
levelThreeSpec s registry =
  let -- Attacks with everything and points the trigger's one target slot at
      -- `aim`, FILTERED out of what the engine offered rather than hand-built: a
      -- Recipient of the right object in the wrong shape is dropped at CR 608.2b
      -- with no error. The four Pikers are indistinguishable to a pure answerer,
      -- which is exactly why the aim is pinned by id.
      answering :: ObjectId.ObjectId -> Prompt.Prompt r -> r
      answering aim p = case p of
        Prompt.ChooseTargets _ _ _ sets -> fmap (Set.filter (== Recipient.ToCreature aim) . snd) sets
        _ -> S.aggressiveAnswer p
      atBlockers aim = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (answering aim)
   in Spec.describe s "Level 3 section" $ do
        Spec.it s "CR 716.2a / CR 508.3d the level-3 trigger pumps by each OTHER attacker and grants double strike" $ do
          paladinClass <- S.printingOf s registry "Paladin Class"
          piker <- S.printingOf s registry "Goblin Piker"
          case attackBoard paladinClass piker 3 of
            (target : others, classId, gs) -> do
              let after = atBlockers target gs
              -- The gameplay assertion this unit exists for, and it is first:
              -- 2/1 printed, +1/+1 from the level-2 section, +3/+3 for the three
              -- OTHER attackers. A count including the target reads 7/6.
              Spec.assertEqWith s "the target is 6/5: +3/+3 for the three other attackers, over the level-2 section's +1/+1" (S.powerToughnessOf target after) (Just (6, 5))
              Spec.assertBool s (Projection.hasKeyword Keyword.DoubleStrike target after) "CR 702.4 the target gained double strike"
              -- The other three attackers took the level-2 section's +1/+1 and
              -- nothing else, which is what says the pump reached ONE creature.
              Spec.assertEqWith s "the untargeted attackers are 3/2 -- the level-2 section alone" (fmap (\oid -> S.powerToughnessOf oid after) others) (fmap (const (Just (3, 2))) others)
              Spec.assertBool s (not (any (\oid -> Projection.hasKeyword Keyword.DoubleStrike oid after) others)) "and none of them gained double strike"
              -- The preconditions the readings above rest on.
              Spec.assertEqWith s "CR 716.2b the Class really is level 3" (levelOf classId after) (Just (ClassLevel.MkClassLevel 3))
              Spec.assertEqWith s "CR 508.1b all four Pikers really were declared attacking bob" (Combat.Type.attackers (GameState.combat after)) (Map.fromList (fmap (\oid -> (oid, AttackTarget.OfPlayer S.bob)) (target : others)))
            _ -> Spec.assertFailure s "fixture should give alice four Pikers and a Class"
        -- The same board with the level as the only difference. CR 716.2a gates
        -- the section on "level N or greater", so at level 2 there is no trigger
        -- at all -- and the level-2 section's own +1/+1 is still on, which is
        -- what says the board is otherwise identical rather than broken.
        Spec.it s "CR 716.2a the level-3 section is off while the Class is level 2" $ do
          paladinClass <- S.printingOf s registry "Paladin Class"
          piker <- S.printingOf s registry "Goblin Piker"
          case attackBoard paladinClass piker 2 of
            (target : others, classId, gs) -> do
              let after = atBlockers target gs
              Spec.assertEqWith s "the would-be target is 3/2 -- the level-2 section and nothing more" (S.powerToughnessOf target after) (Just (3, 2))
              Spec.assertBool s (not (Projection.hasKeyword Keyword.DoubleStrike target after)) "and it gained no double strike"
              Spec.assertEqWith s "CR 716.2b the Class really is level 2" (levelOf classId after) (Just (ClassLevel.MkClassLevel 2))
              Spec.assertEqWith s "CR 508.1b and all four Pikers really did attack" (Combat.Type.attackers (GameState.combat after)) (Map.fromList (fmap (\oid -> (oid, AttackTarget.OfPlayer S.bob)) (target : others)))
            _ -> Spec.assertFailure s "fixture should give alice four Pikers and a Class"

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

-- CR 716.2b: "A level is a designation that any permanent can have. A Class
-- retains its level even if it stops being a Class."
--
-- Song of the Dryads is what makes a Class stop being one: "Enchanted
-- permanent is a colorless Forest land" is a Modification.SetCardType, and CR
-- 205.1a's third clause then takes the Class subtype away with the Enchantment
-- card type it correlates with (CR 205.3h). It and Gliding Licid are the corpus's
-- two SetCardType cards (grep it over data/cards/), and the Licid's sets
-- Enchantment on itself, so this is the one board in the pool on which a Class
-- stops being one.
--
-- The retention is observable across the ROUND TRIP rather than during it, and
-- that is a rules fact rather than a shortcut: the same Aura's SetLandSubtype
-- fires CR 305.7, which strips the permanent's abilities, so while the Song is on
-- it the level-2 section is off no matter what the level says, and Paladin Class
-- is the only card in data/cards/ whose text measures a level at all (grep
-- ClassLevel over the corpus). So the case asserts the strip too --
-- the Piker is 2/1 while the Class is a Forest land, for rule 305.7's reason and
-- not for rule 716.2b's, which the level assertion beside it is what shows.
--
-- The Class is levelled to 2 BEFORE the Song arrives, and the Piker's 3/2 once
-- the Song moves off is the reading: an engine that discarded the level when the
-- permanent stopped being a Class leaves it at CR 716.2d's default of 1 there and
-- the Piker its printed 2/1. Levelling first is what makes the two readings
-- differ -- on an unlevelled Class both report 1.
--
-- A state-based pass runs while the permanent is not a Class, so a wipe placed
-- there would be caught rather than skipped over.

-- The three boards rule 716.2b's sentence needs, in order: the levelled Class
-- untouched, the same Class with the Song on it and the SBAs settled, and the
-- Song moved off onto the Mountain. Each differs from the one before it in
-- exactly one thing, and the level is written once, before any of them.
--
-- Tagged ToObject, which is what casting Song of the Dryads stores: its enchant
-- slot is a Pool.Permanents one, so Target's candidates are ToObject rather than
-- Pawl.Support.attach's ToCreature -- and the host here is an enchantment.
retentionBoards ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState, GameState.GameState, GameState.GameState)
retentionBoards paladinClass plains piker mountain song =
  let (classId, pikerId, gs) = board paladinClass plains piker
      levelled = gainLevel classId gs
      (mountainId, withMountain) = S.addCreature mountain S.alice levelled
      (songId, unattached) = S.addCreature song S.alice withMountain
      enchanted = S.settleSba (S.attachTo songId (Recipient.ToObject classId) unattached)
   in (classId, pikerId, unattached, enchanted, S.attachTo songId (Recipient.ToObject mountainId) enchanted)

-- CR 716.2b's last sentence: "Levels are not a copiable characteristic." CR 707.2
-- is the list this is an exclusion from -- the copiable values are the ones
-- derived from the printed text -- and CR 716.2d is what the copy reads instead,
-- a permanent with no level being treated as level 1.
--
-- Copy Enchantment {2}{U} ("You may have this enchantment enter as a copy of any
-- enchantment on the battlefield") is the producer: its EntryR AsCopy carries
-- `eligible = HasCardType Enchantment`, so a Class on the battlefield is offered
-- where Clone's "any creature" would not offer one.
--
-- The OBSERVABLE is CR 716.2a's activated half rather than its static one: which
-- level bar the copy may activate, and the level that activation lands on. The
-- original is levelled to 2 FIRST, so the two readings come apart -- a copy at CR
-- 716.2d's default of 1 is offered the {2}{W} bar and reaches level 2, while a
-- copy carrying the original's level would be offered the {4}{W} bar and reach
-- level 3. On an UNLEVELLED original both readings say the same thing, which is
-- why the level is written before the copy is made.
--
-- Not implemented: a copy does not acquire the copied object's STATIC abilities
-- (CR 707.2a) -- Pawl.Types.ProjectedCharacteristics carries the other three
-- ability lists and no static one, so Pawl.Engine.Projection.permanentParts reads
-- Face.staticAbilities off the PRINTED face (#2177). That is why the level-2
-- section's +1/+1 is not what this case reads: the copy grants nothing whatever
-- its level says. The Piker's P/T is asserted anyway, as a fence rather than as
-- coverage -- it is the right answer for rule 716.2b's reason as well, and it
-- becomes load-bearing the moment #2177 lands.

-- CR 707.5's copy choice, pinned to one named permanent rather than searched --
-- Pawl.CopySpec's copyNamed. An answerer that hunted for a legal enchantment
-- would find the Class again after a mutation and repair the assertion.
copyNamed :: ObjectId.ObjectId -> Prompt.Prompt r -> r
copyNamed wanted p = case p of
  Prompt.ChooseCopyTarget {} -> Just wanted
  _ -> S.identityAnswer p

-- The levelled board and the same board after alice's Copy Enchantment has
-- resolved as a copy of the Class, plus whatever entered the battlefield in
-- between -- a set difference rather than a name match, so an entry that split
-- into two permanents could not be mistaken for the one copy.
--
-- Put on the stack and resolved rather than cast: CR 707.5's choice is made as
-- the permanent ENTERS (CR 614.12a), which Stack.resolveTop reaches, and paying
-- {2}{U} from Plains would need a second colour on a board whose whole mana
-- supply is there to make the level bars affordable.
copyBoards ::
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  Printing.Printing ->
  (ObjectId.ObjectId, ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState, GameState.GameState)
copyBoards paladinClass plains piker copyEnchantment =
  let (classId, pikerId, gs) = board paladinClass plains piker
      levelled = gainLevel classId gs
      (_, staged) = S.spellOnStack copyEnchantment S.alice levelled
      copied = S.settleSba (S.runPure (copyNamed classId) staged Stack.resolveTop)
      entered = Set.toList (Set.difference (GameState.battlefield copied) (GameState.battlefield levelled))
   in (classId, pikerId, entered, levelled, copied)

designationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
designationSpec s registry = Spec.describe s "Level designation" $ do
  Spec.it s "CR 716.2b a Class retains its level even if it stops being a Class" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    song <- S.printingOf s registry "Song of the Dryads"
    let (classId, pikerId, unattached, enchanted, moved) = retentionBoards paladinClass plains piker mountain song
    -- The gameplay-level assertion the case exists for, first: the level-2
    -- section is on again once the permanent is a Class again, which it can only
    -- be if the level outlived the stretch in which it was not one.
    Spec.assertEqWith s "CR 716.2b the Piker is 3/2 again once the Aura moves off" (S.powerToughnessOf pikerId moved) (Just (3, 2))
    Spec.assertEqWith s "and the level it resumes at is the one it retained" (levelOf classId moved) (Just (ClassLevel.MkClassLevel 2))
    -- What the stretch itself looks like, and the preconditions the assertions
    -- above rest on: were the subtype not stripped, nothing here would be about
    -- rule 716.2b at all.
    Spec.assertEqWith s "before: an Enchantment -- Class, level 2, with its section on" (Projection.cardTypesOf classId unattached, Projection.subtypesOf classId unattached, S.powerToughnessOf pikerId unattached) (Set.singleton CardType.Enchantment, Set.singleton Subtype.Class, Just (3, 2))
    Spec.assertEqWith s "CR 205.1a: Land REPLACES Enchantment, and the Class subtype goes with the type it correlates with" (Projection.cardTypesOf classId enchanted, Projection.subtypesOf classId enchanted) (Set.singleton CardType.Land, Set.singleton Subtype.Forest)
    Spec.assertEqWith s "CR 305.7: the section is off while it is a Forest land because its abilities are stripped" (S.powerToughnessOf pikerId enchanted) (Just (2, 1))
    Spec.assertEqWith s "CR 716.2b: and NOT because the level went anywhere" (levelOf classId enchanted) (Just (ClassLevel.MkClassLevel 2))
    Spec.assertEqWith s "with the Aura gone it is an Enchantment -- Class once more" (Projection.cardTypesOf classId moved, Projection.subtypesOf classId moved) (Set.singleton CardType.Enchantment, Set.singleton Subtype.Class)
  Spec.it s "CR 716.2b levels are not a copiable characteristic" $ do
    paladinClass <- S.printingOf s registry "Paladin Class"
    plains <- S.printingOf s registry "Plains"
    piker <- S.printingOf s registry "Goblin Piker"
    copyEnchantment <- S.printingOf s registry "Copy Enchantment"
    let (classId, pikerId, entered, levelled, copied) = copyBoards paladinClass plains piker copyEnchantment
    case entered of
      [copyId] -> do
        -- The gameplay-level assertion the case exists for, first: the bar the
        -- copy is offered is the one a level-1 Class has, and activating it lands
        -- on 2. A copy that had inherited the original's level 2 would be offered
        -- the {4}{W} bar instead and land on 3.
        Spec.assertEqWith s "CR 716.2d the copy's own first bar takes it to level 2, not to 3" (levelOf copyId (gainLevel copyId copied)) (Just (ClassLevel.MkClassLevel 2))
        Spec.assertEqWith s "CR 716.2b: and no level designation came across with the copy" (levelOf copyId copied) Nothing
        Spec.assertEqWith s "CR 707.2 writes the COPY: the original still holds the level it gained" (levelOf classId copied) (Just (ClassLevel.MkClassLevel 2))
        -- The preconditions the assertions above rest on: the copy really is a
        -- Paladin Class, with a bar to offer at all. Without them a Copy
        -- Enchantment that copied NOTHING would read the same way.
        Spec.assertEqWith s "the copy IS an Enchantment -- Class (CR 205.3h)" (Projection.cardTypesOf copyId copied, Projection.subtypesOf copyId copied) (Set.singleton CardType.Enchantment, Set.singleton Subtype.Class)
        Spec.assertEqWith s "and exactly one bar is offered on it" (barsOffered copyId copied) 1
        -- The fence #2177 will make load-bearing; see the note above. One
        -- level-2 section is granting +1/+1 here, not two.
        Spec.assertEqWith s "the Piker is 3/2, not 4/3" (S.powerToughnessOf pikerId copied) (Just (3, 2))
      _ -> Spec.assertFailure s "the Copy Enchantment did not enter the battlefield as the one new permanent"
    Spec.assertEqWith s "before the copy: the levelled Class alone already makes the Piker 3/2" (S.powerToughnessOf pikerId levelled) (Just (3, 2))
