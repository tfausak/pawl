-- Covers Pawl.Turn: turn structure, the phase schedule, and the CR 508.8 skips.
module Pawl.TurnSpec where

import qualified Data.Sequence as Seq
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.CombatStep as CombatStep
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Phase as Phase
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

turnTests :: Tasty.TestTree
turnTests =
  Tasty.testGroup
    "Turn"
    [ HU.testCase "firstPhase is the untap step" $
        HU.assertEqual "firstPhase" (Phase.Beginning BeginningStep.Untap) Turn.firstPhase,
      HU.testCase "a turn has twelve steps" $
        HU.assertEqual "twelve" 12 (length Turn.allPhases),
      HU.testCase "firstPhase and laterPhases reconstruct the turn template" $
        HU.assertEqual "reconstruct" (Seq.fromList (drop 1 Turn.allPhases)) Turn.laterPhases,
      HU.testCase "untap and cleanup grant no priority"
        . HU.assertBool "no priority"
        $ not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
          && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup)),
      QC.testProperty "a turn never revisits a phase" $
        QC.property (length Turn.allPhases == length (dedupe Turn.allPhases))
    ]

turnDataTests :: Tasty.TestTree
turnDataTests =
  Tasty.testGroup
    "TurnData"
    [ HU.testCase "advance pops the schedule head into the current phase" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.PrecombatMain,
                  GameState.remaining = Seq.fromList [Phase.Combat CombatStep.BeginningOfCombat, Phase.PostcombatMain]
                }
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "phase" (Phase.Combat CombatStep.BeginningOfCombat) (GameState.phase after)
              HU.assertEqual "remaining" (Seq.fromList [Phase.PostcombatMain]) (GameState.remaining after),
      HU.testCase "advance on an empty schedule hands off the turn" $
        let gs0 = Setup.emptyGame S.bothPlayers
            gs =
              gs0
                { GameState.phase = Phase.Ending EndingStep.Cleanup,
                  GameState.remaining = Seq.empty,
                  GameState.activePlayer = S.alice,
                  GameState.turnNumber = 1
                }
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.advance)
         in do
              HU.assertEqual "new active player" S.bob (GameState.activePlayer after)
              HU.assertEqual "phase reset" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "schedule refilled" Turn.laterPhases (GameState.remaining after)
              HU.assertEqual "turn incremented" 2 (GameState.turnNumber after),
      HU.testCase "a fresh game starts at untap with the rest of the turn scheduled" $
        let gs = Setup.emptyGame S.bothPlayers
         in do
              HU.assertEqual "phase" Turn.firstPhase (GameState.phase gs)
              HU.assertEqual "remaining" Turn.laterPhases (GameState.remaining gs)
    ]

skipTests :: Cards.Cards -> Tasty.TestTree
skipTests cards =
  Tasty.testGroup
    "Skip"
    [ HU.testCase "CR 508.8 dropSkippedCombatSteps removes declare blockers and combat damage" $
        let full =
              Seq.fromList
                [ Phase.Combat CombatStep.DeclareBlockers,
                  Phase.Combat CombatStep.CombatDamage,
                  Phase.Combat CombatStep.EndOfCombat,
                  Phase.PostcombatMain
                ]
            expected = Seq.fromList [Phase.Combat CombatStep.EndOfCombat, Phase.PostcombatMain]
         in HU.assertEqual "dropped" expected (Turn.dropSkippedCombatSteps full),
      HU.testCase "CR 508.8 no attacker declared skips to end of combat" $
        -- Nobody has a creature, so no attackers are declared: the declare
        -- blockers and combat damage steps must not run at all.
        let (gs, _, _) = S.combatBoardOf [] []
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "jumped past the two dead steps" (Phase.Combat CombatStep.EndOfCombat) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker keeps the declare blockers step" $
        -- The control: with an attacker, the step after declare attackers is
        -- declare blockers, exactly as before. So the skip is not "always skip".
        let (gs, _, _) = S.combatBoardOf [Cards.pikerPrinting cards] []
            after = snd (Engine.runGamePure S.aggressiveAnswer gs Engine.runStep)
         in HU.assertEqual "declare blockers still next" (Phase.Combat CombatStep.DeclareBlockers) (GameState.phase after),
      HU.testCase "CR 508.8 an attacker-less combat changes no life total" $
        -- End to end: run the whole combat region. No attackers means no damage,
        -- and the turn still leaves combat cleanly.
        let (gs, _, _) = S.combatBoardOf [] []
            after = S.runCombat S.aggressiveAnswer gs
         in do
              HU.assertEqual "bob untouched" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "alice untouched" (Just 20) (S.lifeOf S.alice after)
              HU.assertBool "left combat" (not (S.inCombatPhase (GameState.phase after))),
      HU.testCase "CR 508.8 the skip stands even when an instant could have been cast" $
        -- bob holds a castable Bolt; nobody attacks. The blockers and damage
        -- steps are still dropped -- the priority windows an instant would use
        -- in them do not exist (CR 500.11: proceed as though they don't).
        let (base, _) = S.boltInHand (Cards.mountainPrinting cards) (Cards.lightningBoltPrinting cards) 1 (Phase.Combat CombatStep.DeclareAttackers)
            armed = base {GameState.activePlayer = S.bob}
            after = snd (Engine.runGamePure S.identityAnswer armed (Engine.runTurnBasedActions (Phase.Combat CombatStep.DeclareAttackers)))
            remaining = foldr (:) [] (GameState.remaining after)
         in do
              HU.assertBool "no blockers step" (notElem (Phase.Combat CombatStep.DeclareBlockers) remaining)
              HU.assertBool "no damage step" (notElem (Phase.Combat CombatStep.CombatDamage) remaining)
    ]

dedupe :: (Eq a) => [a] -> [a]
dedupe xs = case xs of
  [] -> []
  h : t -> h : dedupe (filter (/= h) t)

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Turn" [turnTests, turnDataTests, skipTests cards]
