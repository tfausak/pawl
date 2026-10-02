{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers CR 716's Class cards, which need no engine subsystem of their own. A
-- class level bar is a keyword ability (CR 716.2) whose meaning rule 716.2a spells
-- out in full: the activated half is the card's activated ability written with
-- Keyword.ClassLevel on it, which adds rule 716.2a's two riders as
-- ActivationRestriction.OnlyIf and ActivationRestriction.SorcerySpeed
-- (Pawl.Engine.Keyword.printedRiders), and the static half is an ordinary
-- StaticAbility with a CR 604.2 clause. The mark both halves read is
-- Object.classLevel, written by Effect.SetClassLevel and read by
-- Quantity.ClassLevel.
--
-- The "activate only if this Class is level N-1" rider is a RESTRICTION and not
-- ActivatedAbility.condition's grant gate, which would make the bar absent below
-- the level rather than present and prohibited (CR 602.5 prohibits activating an
-- ability the object still HAS). barsHadSpec below is what tells the two
-- readings apart.
--
-- So what this file exercises is that mark's lifecycle: Pawl.Engine.Resolve's
-- SetClassLevel arm writes it, Pawl.Engine.Quantity's ClassLevel arm reads it back
-- through Pawl.Engine.Filter's view, Pawl.Engine.ActivationRestriction's OnlyIf
-- arm gates the next bar's activation on it, and Pawl.Engine.Projection.gatherStatic
-- gates the section's continuous effect on it.
--
-- Paladin Class, AFR 29, is the card under test for every group but the last,
-- and it was picked
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
-- CR 716.2c's "to gain a Class level" needs Sorcerer Class, and
-- gainClassLevelSpec below is what proves it.
--
-- CR 716's "when this Class becomes level N" trigger needs a second card, and
-- becomesLevelSpec below is what proves it: Stormchaser's Talent, BLB, joins
-- Paladin Class for that one group.
module Pawl.ClassSpec where

import qualified Control.Monad as Monad
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action.Type
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Combat as Combat.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.DamageEvent as DamageEvent
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
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Class" $ do
  topSectionSpec s registry
  sectionSpec s registry
  levelThreeSpec s registry
  ladderSpec s registry
  barsHadSpec s registry
  designationSpec s registry
  becomesLevelSpec s registry
  gainClassLevelSpec s registry

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
  let (_, withClass) = S.addPermanent paladinClass S.alice (Setup.emptyGame S.bothPlayers)
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
    let (_, withClass) = S.addPermanent paladinClass S.alice (Setup.emptyGame S.bothPlayers)
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
  let (classId, withClass) = S.addPermanent paladinClass S.alice (S.landsInPlay plains 12)
      (pikerId, withPiker) = S.addPermanent piker S.alice withClass
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
barsOffered oid gs =
  length
    ( do
        Action.Type.Activate o _ <- Action.legalActions S.alice gs
        Monad.guard (o == oid)
        pure ()
    )

-- Activate the first bar alice is OFFERED on this permanent and resolve it.
-- Offered rather than merely present: CR 716.2a leaves every bar on the Class at
-- every level and CR 602.5's riders are what pick one out, so the enumeration is
-- the only thing that knows which. Stack.resolveTop rather than the priority
-- loop: the narrowest path that shows the write, with no settle in between that
-- could sweep something.
gainLevel :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
gainLevel oid gs = case [ability | Action.Type.Activate o ability <- Action.legalActions S.alice gs, o == oid] of
  [] -> gs
  ability : _ ->
    let activated = S.runPure S.identityAnswer gs (Activate.activateAbility S.alice oid ability)
     in S.runPure S.identityAnswer activated Stack.resolveTop

-- CR 716.2a's static half: "As long as this Class is level N or greater, it has
-- [abilities]."
sectionSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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
      (classId, withClass) = S.addPermanent paladinClass S.alice gs
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

-- CR 716.2a's rider is "Activate only if this Class is level N-1", and CR 602.5
-- makes that a prohibition on activating an ability the object HAS. The other
-- reading -- ActivatedAbility.condition, which GRANTS the bar only at level N-1 --
-- refuses the same activations, so the two come apart only where a card reads the
-- abilities an object has rather than what it may do (CR 602.1).
--
-- Paladin Class at level THREE is where they differ most sharply: its ladder is
-- spent, so under the grant gate neither bar is among its abilities and the Class
-- has none, while rule 716.2a leaves both bars on it, prohibited.
--
-- Synthetic Ability Audit is the reader -- "Creatures get +1/+0 for each permanent
-- with an activated ability that isn't a mana ability", which is
-- Filter.HasNonManaActivatedAbility over the battlefield. Synthetic because no
-- printing asks that question of an enchantment: Tsabo's Web and Ravager Wurm ask
-- it of LANDS, Magewright's Stone of a creature, and Zirda, the Dawnwaker of the
-- CARDS in a starting deck, which Pawl.Engine.Projection.View.viewOfCard answers
-- off the printed face with no gate to apply, so both readings agree there. The
-- Enigma Jewel's craft, "four or more nonlands with activated abilities", asks
-- Filter.HasActivatedAbility of a permanent, and is the real card that can
-- replace this one.
--
-- bob owns the Piker so that Paladin Class's own level-2 section -- "Creatures you
-- control get +1/+1" -- cannot reach it, leaving the audit the only thing that
-- moves a number; and the audit moves POWER only, so a modification landing on
-- both axes could not pass for it. No land is on the board, so nothing but the
-- Class can carry an activated ability of any kind.
barsHadSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
barsHadSpec s registry = Spec.describe s "Bars the Class has" $ do
  Spec.it s "CR 716.2a / CR 602.1 a Class has its level bars at every level, activatable or not" $ do
    audit <- S.printingOf s registry "Synthetic Ability Audit"
    paladinClass <- S.printingOf s registry "Paladin Class"
    piker <- S.printingOf s registry "Goblin Piker"
    let (topClass, topPiker, topBoard) = auditBoard audit paladinClass piker (Just 3)
        (_, freshPiker, freshBoard) = auditBoard audit paladinClass piker (Just 1)
        (_, lonePiker, loneBoard) = auditBoard audit paladinClass piker Nothing
    Spec.assertEqWith
      s
      "CR 716.2a: bob's Piker is 3/1, the audit counting a level-3 Class whose bars are all prohibited"
      (S.powerToughnessOf topPiker topBoard)
      (Just (3, 1))
    Spec.assertEqWith
      s
      "CR 716.2a: a level-1 Class, whose first bar IS activatable, is counted the same"
      (S.powerToughnessOf freshPiker freshBoard)
      (Just (3, 1))
    Spec.assertEqWith
      s
      "the same board without the Class: the audit counts nothing and the Piker is its printed 2/1"
      (S.powerToughnessOf lonePiker loneBoard)
      (Just (2, 1))
    Spec.assertEqWith s "CR 716.2b: the first board's Class is at level 3" (fmap (`levelOf` topBoard) topClass) (Just (Just (ClassLevel.MkClassLevel 3)))

-- alice's audit and bob's Piker, plus alice's Class at the given level when there
-- is one. The three boards differ in exactly one thing apiece: whether the Class
-- is there at all, and what level it is at. The level is WRITTEN rather than
-- climbed, there being no mana here to climb with, and the case asserts it.
auditBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Maybe Natural.Natural -> (Maybe ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
auditBoard audit paladinClass piker level =
  let (_, withAudit) = S.addPermanent audit S.alice (Setup.emptyGame S.bothPlayers)
      (pikerId, withPiker) = S.addPermanent piker S.bob withAudit
   in case level of
        Nothing -> (Nothing, pikerId, withPiker)
        Just n ->
          let (classId, withClass) = S.addPermanent paladinClass S.alice withPiker
           in (Just classId, pikerId, atLevel classId n withClass)

-- CR 716.2b: "A level is a designation that any permanent can have. A Class
-- retains its level even if it stops being a Class."
--
-- Song of the Dryads is what makes a Class stop being one: "Enchanted
-- permanent is a colorless Forest land" is a Modification.SetCardType, and CR
-- 205.1a's third clause then takes the Class subtype away with the Enchantment
-- card type it correlates with (CR 205.3h). Of the corpus's other SetCardType
-- cards (grep it over data/cards/), Gliding Licid's sets Enchantment on itself
-- and Kenrith's Transformation's enchants a creature, which a Class is not
-- without a second effect, so this is the board on which a Class stops being one.
--
-- The retention is observable across the ROUND TRIP rather than during it, and
-- that is a rules fact rather than a shortcut: the same Aura's SetLandSubtype
-- fires CR 305.7, which strips the permanent's abilities, so while the Song is on
-- it the level-2 section is off no matter what the level says, and no Class in
-- data/cards/ measures a level except through its own sections (grep ClassLevel
-- over the corpus: Paladin Class and Stormchaser's Talent, each reading only
-- itself). So the case asserts the strip too --
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
      (mountainId, withMountain) = S.addPermanent mountain S.alice levelled
      (songId, unattached) = S.addPermanent song S.alice withMountain
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
-- CR 707.2a's half rides the same fixture, in the two cases after this one: the
-- copy DOES acquire the copied object's static and player abilities, which is
-- what makes a second level-2 section reachable at all. The Piker's 3/2 in the
-- case below is still rule 716.2b's answer and not rule 707.2a's -- the copy
-- enters at CR 716.2d's level 1, so its level-2 section is off however the
-- abilities are read -- so it is the LEVELLED copy that tells the two apart.

designationSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
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

-- CR 716.2a's static half at a section that grants a TRIGGERED ability watching
-- the very level change that turned the section on: "When this Class becomes
-- level 2, return target instant or sorcery card from your graveyard to your
-- hand" (Stormchaser's Talent, BLB).
--
-- CR 603.10 is what makes the section see its own arrival. That rule's exception
-- list is exhaustive -- leaves-the-battlefield, sacrifice, a card leaving a
-- graveyard, a public object put into a hand or library, phasing out, becoming
-- unattached, losing control, a countered spell, a player losing, planeswalking
-- away -- and "becomes level N" is on none of it. So the abilities checked
-- against the event are the ones existing immediately AFTER it, which is after
-- CR 716.2a's static half has granted this one.
--
-- Stormchaser's Talent rather than Paladin Class because Paladin Class prints no
-- such trigger. Its whole text is transcribed in
-- data/cards/stormchasers-talent.json: the top section's enters trigger, both
-- bars, the level-2 trigger under test, and the level-3 section's "whenever you
-- cast an instant or sorcery spell".
--
-- The Class is PLACED rather than cast, so its CR 716.3 enters trigger never
-- fires and no Otter token is on the board -- one moving part fewer, and nothing
-- below reads a creature count.
--
-- Lightning Bolt in ALICE's graveyard and Raise Dead in BOB's. Two different
-- cards, both admitted by the slot's own filter (instant, sorcery), so the pool's
-- ZoneScope is the only thing that can tell them apart. With alice's
-- graveyard alone, "from your graveyard" and "from any graveyard" put the same
-- card in the same hand and the case would prove nothing about CR 400.1's
-- per-player zone; with an EMPTY graveyard the trigger has no legal target, CR
-- 603.3d removes it, and "did not trigger" would be indistinguishable from "had
-- nothing to do".
--
-- Twelve Islands: more than both bars together cost ({3}{U} then {5}{U}), so no
-- assertion here can turn on affordability.
--
-- A SECOND Stormchaser's Talent under bob, already at level 2, is what makes the
-- condition's "oid == bearer" observable. Its level-2 section is on, so it really
-- holds the granted trigger throughout; a condition that matched any Class's level
-- change would fire bob's copy off alice's activation and pull bob's Raise Dead
-- out of bob's graveyard. Without it the bearer is the only object on the board
-- with a class level and the identity check has no observer at all.
talentBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
talentBoard talent island bolt raiseDead =
  let (classId, withClass) = S.addPermanent talent S.alice (S.landsInPlay island 12)
      (mineId, withMine) = S.addGraveyardCard bolt S.alice withClass
      (_, withTheirs) = S.addGraveyardCard raiseDead S.bob withMine
      (theirClassId, withTheirClass) = S.addPermanent talent S.bob withTheirs
   in ( classId,
        mineId,
        atLevel
          theirClassId
          2
          withTheirClass
            { GameState.phase = Phase.PrecombatMain,
              GameState.activePlayer = S.alice,
              GameState.priority = Just S.alice
            }
      )

-- The names of the cards in a zone of pid's, in zone order. Identity AND zone in
-- one reading, which a size would not give: CR 400.7 mints a new object as the
-- card moves, so the id the trigger targeted cannot be looked up in the hand
-- afterwards, and a count alone cannot say WHICH card arrived.
namesIn :: Zone.Zone -> PlayerId.PlayerId -> GameState.GameState -> [CardName.CardName]
namesIn zone pid gs = Maybe.mapMaybe (\oid -> fmap S.nameOf (Game.cardOf oid gs)) (Game.zoneMembers zone pid gs)

-- Activate the first bar alice is offered on the Class -- gainLevel's
-- enumeration, for its reason -- and drain the stack. The priority
-- loop rather than Stack.resolveTop, and that is load-bearing: resolving only the
-- top object leaves the level set and the granted trigger not yet placed, which
-- reads exactly like a trigger that never fired.
--
-- The target is FILTERED out of what the engine offered rather than hand-built --
-- a Recipient of the right card in the wrong shape is dropped at CR 608.2b with
-- no error -- and pinned by id, so an answerer cannot repair a mutation by
-- reaching for the other graveyard's card instead.
--
-- Where the pin is NOT on offer, ONE candidate is answered -- the least, so the
-- choice is deterministic, and one because every slot here wants one target. That
-- is what keeps a trigger raised by bob's Class observable: an answerer that
-- filtered every prompt down to alice's card would hand bob's trigger an empty
-- set, CR 603.3d would drop it for want of a legal target, and a condition
-- matching the wrong bearer would look exactly like one that never matched.
climbAiming :: ObjectId.ObjectId -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
climbAiming classId aim gs =
  let pinned = Recipient.ToObject aim
      answering :: Prompt.Prompt r -> r
      answering p = case p of
        Prompt.ChooseTargets _ _ _ sets ->
          fmap
            ( \(_, candidates) ->
                if Set.member pinned candidates
                  then Set.singleton pinned
                  else maybe Set.empty (Set.singleton . fst) (Set.minView candidates)
            )
            sets
        _ -> S.aggressiveAnswer p
   in case [ability | Action.Type.Activate o ability <- Action.legalActions S.alice gs, o == classId] of
        [] -> gs
        ability : _ ->
          let activated = S.runPure answering gs (Activate.activateAbility S.alice classId ability)
           in S.runPure answering activated Engine.priorityLoop

becomesLevelSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becomesLevelSpec s registry = Spec.describe s "Becomes level trigger" $ do
  Spec.it s "CR 716.2a / CR 603.10 the level-2 section's trigger fires on the very activation that grants it" $ do
    talent <- S.printingOf s registry "Stormchaser's Talent"
    island <- S.printingOf s registry "Island"
    bolt <- S.printingOf s registry "Lightning Bolt"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (classId, mineId, gs) = talentBoard talent island bolt raiseDead
        after = climbAiming classId mineId gs
    -- The gameplay-level assertion the whole unit exists for, and it is first.
    -- An engine that records no level change, or one whose condition never
    -- matches, leaves this hand empty.
    Spec.assertEqWith s "alice's hand holds the Lightning Bolt the trigger returned" (namesIn Zone.Hand S.alice after) [CardName.MkCardName (Text.pack "Lightning Bolt")]
    Spec.assertEqWith s "and her graveyard is empty" (namesIn Zone.Graveyard S.alice after) []
    -- CR 400.1's per-player zone. Bob's card is admitted by the slot's filter and
    -- excluded only by "your graveyard", so a trigger that reached any graveyard
    -- could have taken this one.
    Spec.assertEqWith s "CR 400.1 bob's Raise Dead never left bob's graveyard" (namesIn Zone.Graveyard S.bob after) [CardName.MkCardName (Text.pack "Raise Dead")]
    -- The preconditions the readings above rest on: the level really moved, and
    -- the id the trigger targeted really is gone from the graveyard (CR 400.7
    -- minted a new object in the hand).
    Spec.assertEqWith s "CR 716.2a the level BECAME 2" (levelOf classId after) (Just (ClassLevel.MkClassLevel 2))
    Spec.assertBool s (notElem mineId (Game.zoneMembers Zone.Graveyard S.alice after)) "CR 400.7 the object it was targeted under is gone from the graveyard"
  -- The crossing, on the same board with the level as the only difference. CR
  -- 716.2a's ladder takes a Class from 2 to 3 here, and "becomes level 2" must
  -- not fire again: the condition asks whether the level was BELOW 2 and reached
  -- 2, which an equality on the resulting level cannot answer and a bare
  -- "reached at least 2" gets wrong.
  --
  -- The level-2 section is still ON at level 3 (its gate is "2 or greater"), so
  -- the granted trigger exists throughout -- which is what makes this a statement
  -- about the crossing rather than about the section.
  Spec.it s "CR 716.2a the level-2 trigger does not fire again when the Class goes from 2 to 3" $ do
    talent <- S.printingOf s registry "Stormchaser's Talent"
    island <- S.printingOf s registry "Island"
    bolt <- S.printingOf s registry "Lightning Bolt"
    raiseDead <- S.printingOf s registry "Raise Dead"
    let (classId, mineId, gs) = talentBoard talent island bolt raiseDead
        after = climbAiming classId mineId (atLevel classId 2 gs)
    Spec.assertEqWith s "the Lightning Bolt is still in alice's graveyard" (namesIn Zone.Graveyard S.alice after) [CardName.MkCardName (Text.pack "Lightning Bolt")]
    Spec.assertEqWith s "and her hand is empty" (namesIn Zone.Hand S.alice after) []
    Spec.assertEqWith s "CR 716.2a the level really did move on to 3" (levelOf classId after) (Just (ClassLevel.MkClassLevel 3))

-- CR 716.2c: "to gain a Class level" means "to activate an ability indicated by
-- a class level bar". Sorcerer Class, AFR 233, prints it: at level 2 creatures
-- its controller controls have "{T}: Add {U} or {R}. Spend this mana only to
-- cast an instant or sorcery spell or to gain a Class level." The clause is
-- asked of the ABILITY being activated (Keyword.ClassLevel, which the card
-- writes on each bar), not of its source.
--
-- alice's only mana is the granted ability on five creatures: Shivan Dragon and
-- four Goblin Pikers. The level-3 bar costs {3}{U}{R}, all five; Shivan
-- Dragon's firebreathing costs {R}, and the Mountain board differs from the
-- other in that one land and nothing else.
sorcererBoard :: Printing.Printing -> Printing.Printing -> Printing.Printing -> (ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
sorcererBoard sorcererClass shivan piker =
  let (classId, withClass) = S.addPermanent sorcererClass S.alice (Setup.emptyGame S.bothPlayers)
      (shivanId, withShivan) = S.addPermanent shivan S.alice withClass
      withPikers = iterate (snd . S.addPermanent piker S.alice) withShivan !! 4
   in ( classId,
        shivanId,
        (atLevel classId 2 withPikers)
          { GameState.phase = Phase.PrecombatMain,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
      )

-- The payment's one colour choice, pinned: the red mana comes from Shivan Dragon
-- (the ability's second mode) and blue from everything else, so {3}{U}{R} is
-- paid exactly. identityAnswer would take blue five times.
redFrom :: ObjectId.ObjectId -> Prompt.Prompt r -> r
redFrom redSource p = case p of
  Prompt.ChooseManaYield _ _ oid options | oid == redSource -> NonEmpty.last options
  _ -> S.identityAnswer p

gainClassLevelSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
gainClassLevelSpec s registry = Spec.describe s "Gaining a Class level" $ do
  Spec.it s "CR 716.2c Sorcerer Class's mana pays to gain a Class level" $ do
    sorcererClass <- S.printingOf s registry "Sorcerer Class"
    shivan <- S.printingOf s registry "Shivan Dragon"
    piker <- S.printingOf s registry "Goblin Piker"
    let (classId, shivanId, gs) = sorcererBoard sorcererClass shivan piker
        after = case [ability | Action.Type.Activate o ability <- Action.legalActions S.alice gs, o == classId] of
          [] -> gs
          ability : _ ->
            let activated = S.runPure (redFrom shivanId) gs (Activate.activateAbility S.alice classId ability)
             in S.runPure S.identityAnswer activated Stack.resolveTop
    Spec.assertEqWith s "CR 716.2c the level-3 bar is paid with the creatures' mana" (levelOf classId after) (Just (ClassLevel.MkClassLevel 3))
    Spec.assertEqWith s "and every creature tapped for it" (S.tappedCount S.alice after) 5
  -- The same mana refuses an activation no class level bar indicates; the
  -- Mountain board is the control.
  Spec.it s "CR 106.6 the same mana does not pay Shivan Dragon's firebreathing" $ do
    sorcererClass <- S.printingOf s registry "Sorcerer Class"
    shivan <- S.printingOf s registry "Shivan Dragon"
    piker <- S.printingOf s registry "Goblin Piker"
    mountain <- S.printingOf s registry "Mountain"
    let (_, shivanId, gs) = sorcererBoard sorcererClass shivan piker
    Spec.assertEqWith s "CR 106.6 firebreathing is not offered" (barsOffered shivanId gs) 0
    Spec.assertEqWith s "and one Mountain makes it offered" (barsOffered shivanId (S.landsFor mountain S.alice 1 gs)) 1
  -- Level 3: "Whenever you cast an instant or sorcery spell, that spell deals
  -- damage to each opponent equal to the number of instant and sorcery spells
  -- you've cast this turn." alice casts Goblin Piker, then Divination: two
  -- spells, one of them a sorcery, so bob takes 1 and not 2.
  Spec.it s "CR 716.2a / CR 120.1 the level-3 trigger has the spell deal damage by instants and sorceries cast" $ do
    sorcererClass <- S.printingOf s registry "Sorcerer Class"
    shivan <- S.printingOf s registry "Shivan Dragon"
    piker <- S.printingOf s registry "Goblin Piker"
    divination <- S.printingOf s registry "Divination"
    island <- S.printingOf s registry "Island"
    mountain <- S.printingOf s registry "Mountain"
    let (classId, _, gs0) = sorcererBoard sorcererClass shivan piker
        lands = S.landsFor island S.alice 3 (S.landsFor mountain S.alice 2 (atLevel classId 3 gs0))
        (pikerCardId, withPiker) = S.addHandCard piker S.alice lands
        (divinationId, ready) = S.addHandCard divination S.alice withPiker
        pikerCast = S.runPure S.identityAnswer ready (S.cast S.alice pikerCardId >> Engine.priorityLoop)
        after = S.runPure S.identityAnswer pikerCast (S.cast S.alice divinationId >> Engine.priorityLoop)
        spellIds = [SpellWasCast.spell c | c <- Maybe.mapMaybe Game.castOf (S.eventsOf after), elem CardType.Sorcery (PC.cardTypes (SpellWasCast.characteristics c))]
    Spec.assertEqWith s "bob is dealt 1" (S.lifeOf S.bob after) (Just 19)
    Spec.assertEqWith s "CR 120.1 by Divination, the spell" (fmap DamageEvent.source (S.damageEventsOf after)) spellIds
    Spec.assertEqWith s "and alice cast both spells" (length (Maybe.mapMaybe Game.castOf (S.eventsOf after))) 2
