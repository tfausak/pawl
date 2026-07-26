-- Covers Pawl.Replay: record/replay transcript round-trips.
module Pawl.ReplaySpec where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Numeric.Natural as Natural
import qualified Pawl.Decide as Decide
import qualified Pawl.Engine as Engine
import qualified Pawl.Registry as Registry
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Target as Target
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.CostComponent as CostComponent
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.EntryOption as EntryOption
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.MulliganOffer as MulliganOffer
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Registry as Registry.Type
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.TriggeredAbility as TriggeredAbility
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
          HU.testCase "ChooseX records and replays a Natural" $
            let p = Prompt.ChooseX decider S.alice oid
             in HU.assertEqual "round trip" (Just (4 :: Natural.Natural)) (Replay.decode p (Replay.encode p 4)),
          -- CR 601.2b / 700.2a: a modal choice (Response.ChoseModes, a Set
          -- ModeIndex) round-trips through the DecisionLog exactly like every
          -- other response -- no JSON codec is involved: Response has never had
          -- one (only Prompt/Response answers get serialized, via Replay's
          -- encode/decode, not Pawl.Codec's JSON arms).
          HU.testCase "ChooseModes records and replays a Set ModeIndex" $
            let legal = Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 1]
                p = Prompt.ChooseModes decider S.alice oid legal 1
                answer = Set.singleton (ModeIndex.MkModeIndex 1)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "ChooseCopyTarget records and replays a Maybe ObjectId" $
            let p = Prompt.ChooseCopyTarget decider S.alice oid [ObjectId.MkObjectId 7]
                answer = Just (ObjectId.MkObjectId 7)
             in HU.assertEqual "round-trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          -- CR 208.2b: Primal Plasma is in no deck, so no gameplay-level test
          -- reaches Response.ChoseEntryOption through the record/replay path --
          -- this exercises the transcript codec directly, matching the shape
          -- of every other payload-carrying prompt above.
          HU.testCase "ChooseEntryOption records and replays a Natural" $
            let options = [EntryOption.MkEntryOption {EntryOption.power = 3, EntryOption.toughness = 3, EntryOption.keywords = Set.empty}]
                p = Prompt.ChooseEntryOption decider S.alice oid options
             in HU.assertEqual "round trip" (Just (1 :: Natural.Natural)) (Replay.decode p (Replay.encode p 1)),
          HU.testCase "DeclareMulligan records and replays a MulliganDecision" $
            let offer = MulliganOffer.MkMulliganOffer {MulliganOffer.taken = 0, MulliganOffer.bottomCount = 1}
                p = Prompt.DeclareMulligan decider S.alice offer
             in HU.assertEqual "round trip" (Just MulliganDecision.Mulligan) (Replay.decode p (Replay.encode p MulliganDecision.Mulligan)),
          HU.testCase "Bottom records and replays an ordered [ObjectId]" $
            let p = Prompt.Bottom decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8] 1
                answer = [ObjectId.MkObjectId 8]
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "MulliganAction records and replays a Maybe ObjectId" $
            let p = Prompt.MulliganAction decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]
                answer = Just (ObjectId.MkObjectId 8)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "OpeningHandAction records and replays a Maybe ObjectId" $
            let p = Prompt.OpeningHandAction decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]
                answer = Just (ObjectId.MkObjectId 7)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "defaultAnswer attacks with nothing" $
            HU.assertEqual "no attacks" [] (Replay.defaultAnswer attackPrompt),
          HU.testCase "defaultAnswer blocks with nothing" $
            HU.assertEqual "no blocks" Map.empty (Replay.defaultAnswer blockPrompt),
          HU.testCase "defaultAnswer assigns a LEGAL division" $
            -- Total must equal the attacker's power, or the fallback would be
            -- rejected by validation and deal no damage at all.
            HU.assertEqual "all to one blocker" (Map.singleton (Recipient.ToCreature oid) 2) (Replay.defaultAnswer damagePrompt),
          HU.testCase "OrderTriggers records and replays a permutation" $
            let p = Prompt.OrderTriggers decider S.alice [oid, ObjectId.MkObjectId 8]
                answer = [1, 0] :: [Natural.Natural]
             in HU.assertEqual "round-trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "defaultAnswer keeps the canonical order" $
            HU.assertEqual
              "identity permutation"
              [0, 1 :: Natural.Natural]
              (Replay.defaultAnswer (Prompt.OrderTriggers decider S.alice [oid, ObjectId.MkObjectId 8])),
          HU.testCase "ChooseSacrifices records and replays a Set ObjectId" $
            let p = Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1
                answer = Set.singleton (ObjectId.MkObjectId 8)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "defaultAnswer sacrifices the first `count` offered, in order" $
            HU.assertEqual
              "the ascending prefix"
              (Set.singleton oid)
              (Replay.defaultAnswer (Prompt.ChooseSacrifices decider S.alice oid [oid, ObjectId.MkObjectId 8] 1)),
          HU.testCase "ChooseCost records and replays a Cost" $
            let printed = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []
                alternative = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.SacrificeThis]
                p = Prompt.ChooseCost decider S.alice oid [printed, alternative]
             in HU.assertEqual "round trip" (Just alternative) (Replay.decode p (Replay.encode p alternative)),
          HU.testCase "defaultAnswer takes the first offered cost (the printed one)" $
            let printed = Cost.Type.MkCost (Just (ManaCost.MkManaCost [ManaSymbol.Generic 4])) []
                alternative = Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) [CostComponent.SacrificeThis]
             in HU.assertEqual
                  "the printed one"
                  printed
                  (Replay.defaultAnswer (Prompt.ChooseCost decider S.alice oid [printed, alternative])),
          -- #136 / CR 729.2: "Randomly determine which player goes first." The
          -- determination is randomness, not a choice, so the prompt carries NO
          -- Decider -- Shuffle is the only other such constructor. Recording it
          -- is what keeps a subgame replayable: the randomness lives in the
          -- interpreter, and the transcript carries what it rolled.
          HU.testCase "RandomFirstPlayer round-trips through the transcript" $
            let p = Prompt.RandomFirstPlayer (S.alice NonEmpty.:| [S.bob])
             in HU.assertEqual "round trip" (Just S.bob) (Replay.decode p (Replay.encode p S.bob)),
          HU.testCase "a short transcript starts the head of the turn order" $
            HU.assertEqual
              "the head"
              S.alice
              (Replay.defaultAnswer (Prompt.RandomFirstPlayer (S.alice NonEmpty.:| [S.bob]))),
          -- CR 507.1 / 703.4h: the defending-player choice round-trips like every
          -- other prompt. NonEmpty because the action only runs when there is at
          -- least one candidate.
          HU.testCase "ChooseDefender round-trips through the transcript" $
            let p = Prompt.ChooseDefender decider S.alice (S.bob NonEmpty.:| [S.carol])
             in do
                  HU.assertEqual "carol round trips" (Just S.carol) (Replay.decode p (Replay.encode p S.carol))
                  -- Discriminating: both legs must round-trip. A decode that
                  -- ignored the response and returned the head would pass the
                  -- carol leg only by accident of which one was written first.
                  HU.assertEqual "bob round trips" (Just S.bob) (Replay.decode p (Replay.encode p S.bob)),
          HU.testCase "a first-player roll does not decode as a defender choice" $
            -- Discriminating: this is the assertion that fails if ChooseDefender
            -- reuses Response.DeterminedFirstPlayer instead of getting its own
            -- constructor. Both carry a PlayerId, so the types would not object.
            let p = Prompt.ChooseDefender decider S.alice (S.bob NonEmpty.:| [S.carol])
             in HU.assertEqual "mismatch" Nothing (Replay.decode p (Response.DeterminedFirstPlayer S.bob)),
          -- CR 601.2g: the mana-source choice round-trips like every other prompt.
          HU.testCase "ChooseManaSource round-trips through the transcript" $
            let a = ObjectId.MkObjectId 7
                b = ObjectId.MkObjectId 9
                p = Prompt.ChooseManaSource decider S.alice (a NonEmpty.:| [b])
             in do
                  HU.assertEqual "the second source round trips" (Just b) (Replay.decode p (Replay.encode p b))
                  -- Discriminating for the same reason ChooseDefender's pair is: a
                  -- decode that ignored the response and returned the head would
                  -- pass one leg by accident.
                  HU.assertEqual "the first source round trips" (Just a) (Replay.decode p (Replay.encode p a)),
          HU.testCase "a discard choice does not decode as a mana-source choice" $
            -- Discriminating: this fails if ChooseManaSource reuses another
            -- ObjectId-shaped response instead of getting its own constructor.
            let p = Prompt.ChooseManaSource decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
             in HU.assertEqual "mismatch" Nothing (Replay.decode p (Response.ChoseDiscard [ObjectId.MkObjectId 7])),
          HU.testCase "a short transcript taps the first offered source" $
            HU.assertEqual
              "the head"
              (ObjectId.MkObjectId 7)
              (Replay.defaultAnswer (Prompt.ChooseManaSource decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9]))),
          HU.testCase "a short transcript defends with the first candidate" $
            -- Discriminating against a defaultAnswer that returned the active
            -- player, or a candidate not on the offered list: the first candidate
            -- is always legal, since the prompt is only asked with candidates.
            HU.assertEqual
              "the head"
              S.bob
              (Replay.defaultAnswer (Prompt.ChooseDefender decider S.alice (S.bob NonEmpty.:| [S.carol]))),
          -- #133: the concede channel round-trips like every other prompt. Note
          -- the prompt takes a PlayerId and NO Decider (CR 723.6).
          HU.testCase "Concede round-trips both ways" $
            let p = Prompt.Concede S.alice
             in do
                  HU.assertEqual "concedes" (Just Concession.Concedes) (Replay.decode p (Replay.encode p Concession.Concedes))
                  HU.assertEqual "continues" (Just Concession.Continues) (Replay.decode p (Replay.encode p Concession.Continues)),
          HU.testCase "a short transcript defaults a Concede to Continues" $
            HU.assertEqual "least eventful" Concession.Continues (Replay.defaultAnswer (Prompt.Concede S.alice))
        ]

-- The starting state, the game program, and a transcript recorded with
-- playLandAnswer (whose choices differ from Replay's exhausted-transcript
-- fallback, keeping the assertions below honest: the transcript has to
-- actually carry the decisions).
recordedGame :: Registry.Type.Registry -> IO (GameState.GameState, Game.Type.Game Result.Result, GameState.GameState, [Response.Response])
recordedGame registry = do
  matchup <- S.redRed registry
  let start = Setup.emptyGame (fmap fst matchup)
      game = Engine.playFrom matchup
      ((_, recorded), transcript) = Replay.record S.playLandAnswer start game
  pure (start, game, recorded, transcript)

replayTests :: Registry.Type.Registry -> Tasty.TestTree
replayTests registry =
  Tasty.testGroup
    "Replay"
    [ HU.testCase "replaying a recorded game reproduces the final state" $ do
        (start, game, recorded, transcript) <- recordedGame registry
        HU.assertEqual "final states equal" recorded (snd (Replay.replay transcript start game)),
      HU.testCase "the transcript is what carries the decisions" $ do
        (start, game, recorded, _) <- recordedGame registry
        HU.assertBool "empty log diverges" (recorded /= snd (Replay.replay [] start game)),
      HU.testCase "a recorded goldfish also replays" $ do
        (start, game, _, _) <- recordedGame registry
        let ((_, gf), gfLog) = Replay.record S.identityAnswer start game
        HU.assertEqual "goldfish" gf (snd (Replay.replay gfLog start game)),
      HU.testCase "a ChooseTargets answer round-trips through the transcript" $
        let sets = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Set.singleton (Recipient.ToPlayer S.bob))
            p = Prompt.ChooseTargets (Decider.MkDecider S.alice) S.alice (ObjectId.MkObjectId 0) sets
            answer = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Recipient.ToPlayer S.bob)
         in HU.assertEqual "decode . encode = Just" (Just answer) (Replay.decode p (Replay.encode p answer)),
      -- CR 700.2b/603.3d: the mode chosen as Aether Channeler's ETB trigger is
      -- placed (Engine.placeOne prompts ChooseModes) records/replays exactly
      -- like a spell's ChooseModes -- a Response.ChoseModes round-trips through
      -- the DecisionLog byte-identically.
      HU.testCase "Aether Channeler's trigger ChooseModes records and replays a Set ModeIndex" $ do
        acPrinting <- Registry.printing registry "Aether Channeler"
        let (acId, gs) = S.addCreature acPrinting S.alice (Setup.emptyGame S.bothPlayers)
            decider = Decide.deciderFor S.alice gs
        case Card.Type.triggeredAbilities (Printing.card acPrinting) of
          [ability] -> do
            let legal = Target.fillableModes acId Map.empty (TriggeredAbility.modal ability) gs
                p = Prompt.ChooseModes decider S.alice acId legal 1
                answer = Set.singleton (ModeIndex.MkModeIndex 2)
            HU.assertEqual "legal modes are 0 and 2 (bounce self-excluded)" (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]) legal
            HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer))
          _ -> HU.assertFailure "Aether Channeler must have exactly one triggered ability"
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Replay" [replayTests registry, combatReplayTests]
