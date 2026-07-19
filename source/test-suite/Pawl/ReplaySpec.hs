-- Covers Pawl.Replay: record/replay transcript round-trips.
module Pawl.ReplaySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Cards as Cards
import qualified Pawl.Decide as Decide
import qualified Pawl.Engine as Engine
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.SlotName as SlotName
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

combatReplayTests :: Tasty.TestTree
combatReplayTests =
  let decider = Decide.deciderFor S.alice (Setup.emptyGame S.bothPlayers)
      oid = ObjectId.MkObjectId 7
      attackPrompt = Prompt.DeclareAttackers decider S.alice [oid]
      blockPrompt = Prompt.DeclareBlockers decider S.bob [oid] [oid]
      damagePrompt = Prompt.AssignCombatDamage decider S.alice oid (Map.singleton (Recipient.ToCreature oid) 0) 2
   in Tasty.testGroup
        "CombatReplay"
        [ HU.testCase "attackers round-trip through the transcript" $
            HU.assertEqual "round trip" (Just [oid]) (Replay.decode attackPrompt (Replay.encode attackPrompt [oid])),
          HU.testCase "blockers round-trip through the transcript" $
            let answer = Map.singleton oid oid
             in HU.assertEqual "round trip" (Just answer) (Replay.decode blockPrompt (Replay.encode blockPrompt answer)),
          HU.testCase "a damage assignment round-trips through the transcript" $
            let answer :: Map.Map Recipient.Recipient Natural.Natural
                answer = Map.singleton (Recipient.ToCreature oid) 2
             in HU.assertEqual "round trip" (Just answer) (Replay.decode damagePrompt (Replay.encode damagePrompt answer)),
          HU.testCase "a mismatched response decodes to Nothing" $
            HU.assertEqual "mismatch" Nothing (Replay.decode attackPrompt (Response.Shuffled [oid])),
          HU.testCase "defaultAnswer attacks with nothing" $
            HU.assertEqual "no attacks" [] (Replay.defaultAnswer attackPrompt),
          HU.testCase "defaultAnswer blocks with nothing" $
            HU.assertEqual "no blocks" Map.empty (Replay.defaultAnswer blockPrompt),
          HU.testCase "defaultAnswer assigns a LEGAL division" $
            -- Total must equal the attacker's power, or the fallback would be
            -- rejected by validation and deal no damage at all.
            HU.assertEqual "all to one blocker" (Map.singleton (Recipient.ToCreature oid) 2) (Replay.defaultAnswer damagePrompt)
        ]

replayTests :: Cards.Cards -> Tasty.TestTree
replayTests cards =
  let start = Setup.emptyGame (NonEmpty.map fst (S.redRed cards))
      game = Engine.playFrom (S.redRed cards)
      -- Recorded with playLandAnswer, whose choices differ from Replay's
      -- exhausted-transcript fallback. That keeps these assertions honest: the
      -- transcript has to actually carry the decisions.
      ((_, recorded), transcript) = Replay.record S.playLandAnswer start game
   in Tasty.testGroup
        "Replay"
        [ HU.testCase "replaying a recorded game reproduces the final state" $
            HU.assertEqual "final states equal" recorded (snd (Replay.replay transcript start game)),
          HU.testCase "the transcript is what carries the decisions" $
            HU.assertBool "empty log diverges" $
              recorded /= snd (Replay.replay [] start game),
          HU.testCase "a recorded goldfish also replays" $
            let ((_, gf), gfLog) = Replay.record S.identityAnswer start game
             in HU.assertEqual "goldfish" gf (snd (Replay.replay gfLog start game)),
          HU.testCase "a ChooseTargets answer round-trips through the transcript" $
            let sets = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.singleton (Recipient.ToPlayer S.bob))
                p = Prompt.ChooseTargets (Decider.MkDecider S.alice) S.alice (ObjectId.MkObjectId 0) sets
                answer = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Recipient.ToPlayer S.bob)
             in HU.assertEqual "decode . encode = Just" (Just answer) (Replay.decode p (Replay.encode p answer))
        ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Replay" [replayTests cards, combatReplayTests]
