{-# LANGUAGE GADTs #-}

-- Covers Pawl.Replay: record/replay transcript round-trips.
module Pawl.ReplaySpec where

import qualified Data.List as List
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
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Concession as Concession
import qualified Pawl.Types.Cost as Cost.Type
import qualified Pawl.Types.CostComponent as CostComponent
import qualified Pawl.Types.Decider as Decider
import qualified Pawl.Types.Desync as Desync
import qualified Pawl.Types.EntryOption as EntryOption
import qualified Pawl.Types.EntwineDecision as EntwineDecision
import qualified Pawl.Types.Game as Game.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.ManaUnit as ManaUnit
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.MulliganDecision as MulliganDecision
import qualified Pawl.Types.MulliganOffer as MulliganOffer
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.PhyrexianPayment as PhyrexianPayment
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.Response as Response
import qualified Pawl.Types.Result as Result
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The one-unit yield of a single-colour mana ability, which is what a
-- ChooseManaYield candidate looks like for every source but Sol Ring.
oneMana :: Color.Color -> Mana.Mana
oneMana color = Mana.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored color}]

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
          -- The recorded answer is 4 while the prompt's bound is 2: the transcript
          -- carries what the player SAID, and CR 601.2b lets them say more than
          -- the board can pay. A codec that folded the bound into the response
          -- would replay a different game (#417).
          HU.testCase "ChooseX records and replays a Natural" $
            let p = Prompt.ChooseX decider S.alice oid 2
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
          -- CR 702.42a: whether the entwine cost was paid decides how many modes
          -- the spell has, so a transcript that lost it would replay a different
          -- spell. Both answers are checked: a decode that ignored the response
          -- and returned a fixed value would pass one leg by accident.
          HU.testCase "ChooseEntwine records and replays an EntwineDecision" $
            let entwineCost =
                  Cost.Type.MkCost
                    { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
                      Cost.Type.components = []
                    }
                p = Prompt.ChooseEntwine decider S.alice oid entwineCost
             in do
                  HU.assertEqual
                    "entwining round trips"
                    (Just EntwineDecision.Entwines)
                    (Replay.decode p (Replay.encode p EntwineDecision.Entwines))
                  HU.assertEqual
                    "declining round trips"
                    (Just EntwineDecision.Declines)
                    (Replay.decode p (Replay.encode p EntwineDecision.Declines))
                  -- Discriminating: fails if ChooseEntwine reuses another
                  -- two-valued response rather than getting its own constructor.
                  HU.assertEqual
                    "an optional decision is not an entwine announcement"
                    Nothing
                    (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises)),
          HU.testCase "a short transcript declines entwine" $
            -- CR 702.42a: declining is always legal and costs nothing, so it is
            -- the least-eventful fallback when a transcript runs short.
            HU.assertEqual
              "declines"
              EntwineDecision.Declines
              ( Replay.defaultAnswer
                  ( Prompt.ChooseEntwine
                      decider
                      S.alice
                      oid
                      Cost.Type.MkCost
                        { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 1]),
                          Cost.Type.components = []
                        }
                  )
              ),
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
          -- CR 603.5: both answers to a printed "may", so the transcript is
          -- proved to distinguish them -- a codec that collapsed them would
          -- replay a declined Renewed Faith as a taken one.
          HU.testCase "ChooseOptional records and replays both answers" $
            let p = Prompt.ChooseOptional decider S.alice oid (ModeIndex.MkModeIndex 0)
             in do
                  HU.assertEqual "exercised" (Just OptionalDecision.Exercises) (Replay.decode p (Replay.encode p OptionalDecision.Exercises))
                  HU.assertEqual "declined" (Just OptionalDecision.Declines) (Replay.decode p (Replay.encode p OptionalDecision.Declines)),
          -- CR 603.5: a transcript that runs short must not silently take an
          -- option its author never chose.
          HU.testCase "defaultAnswer declines a may" $
            HU.assertEqual
              "declines"
              OptionalDecision.Declines
              (Replay.defaultAnswer (Prompt.ChooseOptional decider S.alice oid (ModeIndex.MkModeIndex 0))),
          HU.testCase "a mismatched response does not decode as a may" $
            HU.assertEqual
              "mismatch"
              Nothing
              (Replay.decode (Prompt.ChooseOptional decider S.alice oid (ModeIndex.MkModeIndex 0)) (Response.Conceded Concession.Continues)),
          HU.testCase "defaultAnswer attacks with nothing" $
            HU.assertEqual "no attacks" [] (Replay.defaultAnswer attackPrompt),
          HU.testCase "defaultAnswer blocks with nothing" $
            HU.assertEqual "no blocks" Map.empty (Replay.defaultAnswer blockPrompt),
          HU.testCase "defaultAnswer assigns a LEGAL division" $
            -- Total must equal the attacker's power, or the fallback would be
            -- rejected by validation and deal no damage at all.
            HU.assertEqual "all to one blocker" (Map.singleton (Recipient.ToCreature oid) 2) (Replay.defaultAnswer damagePrompt),
          -- The payload mixes both kinds of entry (CR 113.7's borne trigger and
          -- CR 725.2's sourceless one), which is what the batch really looks like
          -- when the monarch controls a trigger of her own at the same moment.
          HU.testCase "OrderTriggers records and replays a permutation" $
            let p = Prompt.OrderTriggers decider S.alice [TriggerSource.OfObject oid, TriggerSource.Sourceless]
                answer = [1, 0] :: [Natural.Natural]
             in HU.assertEqual "round-trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
          HU.testCase "defaultAnswer keeps the canonical order" $
            HU.assertEqual
              "identity permutation"
              [0, 1 :: Natural.Natural]
              (Replay.defaultAnswer (Prompt.OrderTriggers decider S.alice [TriggerSource.OfObject oid, TriggerSource.Sourceless])),
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
          -- CR 605.3b / 105.4: the mana an any-colour source was tapped for is a
          -- decision, so it has to survive a transcript like any other.
          HU.testCase "ChooseManaYield round-trips through the transcript" $
            let black = oneMana Color.Black
                red = oneMana Color.Red
                p = Prompt.ChooseManaYield decider S.alice (ObjectId.MkObjectId 7) (black NonEmpty.:| [red])
             in do
                  HU.assertEqual "black round trips" (Just black) (Replay.decode p (Replay.encode p black))
                  -- Discriminating for the same reason the pair above is: a decode
                  -- that returned the head would pass one leg by accident.
                  HU.assertEqual "red round trips" (Just red) (Replay.decode p (Replay.encode p red)),
          HU.testCase "a mana-source choice does not decode as a mana-yield choice" $
            -- Discriminating: this fails if ChooseManaYield reuses ChoseManaSource
            -- rather than getting its own constructor.
            let p = Prompt.ChooseManaYield decider S.alice (ObjectId.MkObjectId 7) (oneMana Color.Black NonEmpty.:| [oneMana Color.Red])
             in HU.assertEqual "mismatch" Nothing (Replay.decode p (Response.ChoseManaSource (ObjectId.MkObjectId 7))),
          -- CR 701.34a: who was proliferated onto is a decision, so it has to
          -- survive a transcript like any other.
          HU.testCase "ChooseProliferate round-trips through the transcript" $
            let a = ObjectId.MkObjectId 7
                p = Prompt.ChooseProliferate decider S.alice [a] [S.bob]
                both = (Set.singleton a, Set.singleton S.bob)
                neither = (Set.empty, Set.empty)
             in do
                  HU.assertEqual "taking both round trips" (Just both) (Replay.decode p (Replay.encode p both))
                  -- Discriminating: CR 701.34a's "any number" includes none, and a
                  -- decode that defaulted to the offered set would pass one leg.
                  HU.assertEqual "declining round trips" (Just neither) (Replay.decode p (Replay.encode p neither)),
          HU.testCase "a short transcript proliferates onto nothing" $
            HU.assertEqual
              "declines"
              (Set.empty, Set.empty)
              (Replay.defaultAnswer (Prompt.ChooseProliferate decider S.alice [ObjectId.MkObjectId 7] [S.bob])),
          -- CR 704.5j: which legend its controller kept is a decision, so it has to
          -- survive a transcript like any other.
          HU.testCase "ChooseLegend round-trips through the transcript" $
            let a = ObjectId.MkObjectId 7
                b = ObjectId.MkObjectId 9
                p = Prompt.ChooseLegend decider S.alice (a NonEmpty.:| [b])
             in do
                  HU.assertEqual "keeping the second round trips" (Just b) (Replay.decode p (Replay.encode p b))
                  -- Discriminating: a decode that ignored the response and returned
                  -- the head would pass one leg by accident.
                  HU.assertEqual "keeping the first round trips" (Just a) (Replay.decode p (Replay.encode p a)),
          HU.testCase "a legend choice does not decode as a mana-source choice" $
            -- Discriminating: fails if ChooseLegend reuses ChoseManaSource rather
            -- than getting its own ObjectId-shaped constructor.
            let p = Prompt.ChooseLegend decider S.alice (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
             in HU.assertEqual "mismatch" Nothing (Replay.decode p (Response.ChoseManaSource (ObjectId.MkObjectId 7))),
          -- CR 603.7c: which of several minted tokens "it" names is a decision, so
          -- it has to survive a transcript like any other.
          HU.testCase "ChooseBoundToken round-trips through the transcript" $
            let a = ObjectId.MkObjectId 7
                b = ObjectId.MkObjectId 9
                p = Prompt.ChooseBoundToken decider S.alice oid (a NonEmpty.:| [b])
             in do
                  HU.assertEqual "binding the second round trips" (Just b) (Replay.decode p (Replay.encode p b))
                  -- Discriminating: a decode that ignored the response and returned
                  -- the head would pass one leg by accident.
                  HU.assertEqual "binding the first round trips" (Just a) (Replay.decode p (Replay.encode p a)),
          HU.testCase "a bound-token choice does not decode as a legend choice" $
            -- Discriminating: fails if ChooseBoundToken reuses ChoseLegend rather
            -- than getting its own ObjectId-shaped constructor.
            let p = Prompt.ChooseBoundToken decider S.alice oid (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9])
             in HU.assertEqual "mismatch" Nothing (Replay.decode p (Response.ChoseLegend (ObjectId.MkObjectId 7))),
          HU.testCase "a short transcript binds the first token minted" $
            -- CR 603.7c: every minted token is a legal referent, so the head is
            -- legal -- and it is what the engine bound before the choice existed.
            HU.assertEqual
              "the head"
              (ObjectId.MkObjectId 7)
              (Replay.defaultAnswer (Prompt.ChooseBoundToken decider S.alice oid (ObjectId.MkObjectId 7 NonEmpty.:| [ObjectId.MkObjectId 9]))),
          -- CR 118.13a: which way a Phyrexian mana symbol was announced to be
          -- paid is a decision made as the spell is proposed, so it has to
          -- survive a transcript like any other.
          HU.testCase "AnnouncePhyrexianPayment round-trips through the transcript" $
            let p =
                  Prompt.AnnouncePhyrexianPayment
                    decider
                    S.alice
                    oid
                    Color.Green
                    (PhyrexianPayment.PaysMana NonEmpty.:| [PhyrexianPayment.PaysLife])
             in do
                  HU.assertEqual
                    "the life route round trips"
                    (Just PhyrexianPayment.PaysLife)
                    (Replay.decode p (Replay.encode p PhyrexianPayment.PaysLife))
                  -- Discriminating for the same reason the pairs above are: a
                  -- decode that ignored the response and returned the head would
                  -- pass one leg by accident.
                  HU.assertEqual
                    "the mana route round trips"
                    (Just PhyrexianPayment.PaysMana)
                    (Replay.decode p (Replay.encode p PhyrexianPayment.PaysMana)),
          HU.testCase "an optional decision does not decode as a Phyrexian announcement" $
            -- Discriminating: fails if AnnouncePhyrexianPayment reuses another
            -- two-valued response (ChoseOptional, Conceded, DeclaredMulligan)
            -- rather than getting its own constructor.
            let p =
                  Prompt.AnnouncePhyrexianPayment
                    decider
                    S.alice
                    oid
                    Color.Green
                    (PhyrexianPayment.PaysMana NonEmpty.:| [PhyrexianPayment.PaysLife])
             in HU.assertEqual "mismatch" Nothing (Replay.decode p (Response.ChoseOptional OptionalDecision.Exercises)),
          HU.testCase "a short transcript announces the first offered Phyrexian route" $
            -- Every offered route is payable (the prompt is raised only with two
            -- payable routes), so the head is a legal answer.
            HU.assertEqual
              "the head"
              PhyrexianPayment.PaysLife
              ( Replay.defaultAnswer
                  ( Prompt.AnnouncePhyrexianPayment
                      decider
                      S.alice
                      oid
                      Color.Green
                      (PhyrexianPayment.PaysLife NonEmpty.:| [PhyrexianPayment.PaysMana])
                  )
              ),
          HU.testCase "a short transcript produces the first offered mana yield" $
            HU.assertEqual
              "the head"
              (oneMana Color.Black)
              (Replay.defaultAnswer (Prompt.ChooseManaYield decider S.alice (ObjectId.MkObjectId 7) (oneMana Color.Black NonEmpty.:| [oneMana Color.Red]))),
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
            -- NOT "least eventful", unlike the arms around it: dropping a
            -- concession hands the win to the other player (CR 104.2a), which
            -- is why Replay.replay reports the desync.
            HU.assertEqual "the game keeps running" Concession.Continues (Replay.defaultAnswer (Prompt.Concede S.alice))
        ]

-- Concedes whenever asked, and otherwise takes the identity answer. Delegating
-- through a wildcard keeps this out of the -Werror exhaustiveness net.
concedeAnswer :: Prompt.Prompt r -> r
concedeAnswer p = case p of
  Prompt.Concede _ -> Concession.Concedes
  _ -> S.identityAnswer p

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
        let ((_, replayed), desync) = Replay.replay transcript start game
        HU.assertEqual "final states equal" recorded replayed
        HU.assertEqual "and the transcript answered every prompt" Nothing desync,
      HU.testCase "the transcript is what carries the decisions" $ do
        (start, game, recorded, _) <- recordedGame registry
        let ((_, replayed), desync) = Replay.replay [] start game
        HU.assertBool "empty log diverges" (recorded /= replayed)
        -- The divergence is REPORTED rather than silent. An empty log runs out
        -- at the very first prompt, so the report names index 0.
        HU.assertEqual "and says where it ran out" (Just (Desync.Exhausted 0)) desync,
      HU.testCase "a recorded goldfish also replays" $ do
        (start, game, _, _) <- recordedGame registry
        let ((_, gf), gfLog) = Replay.record S.identityAnswer start game
            ((_, replayed), desync) = Replay.replay gfLog start game
        HU.assertEqual "goldfish" gf replayed
        HU.assertEqual "no desync" Nothing desync,
      -- #144. Pawl.Replay.defaultAnswer is deliberately total, so a transcript
      -- that has drifted out of step with the prompts the engine actually asks
      -- does not crash -- it silently answers everything from the fallback and
      -- plays out a DIFFERENT game. For Prompt.Concede that fallback is
      -- Concession.Continues, so a dropped concession changes who WINS (CR
      -- 104.2a), not merely how a choice was filled. These pin the report that
      -- makes that visible: replay names the first prompt the transcript failed
      -- to answer, and nothing after it can be trusted.
      HU.testCase "a truncated transcript reports where it ran out" $ do
        (start, game, _, transcript) <- recordedGame registry
        let (_, desync) = Replay.replay (List.init transcript) start game
        case desync of
          Just (Desync.Exhausted _) -> pure ()
          _ -> HU.assertFailure ("expected an Exhausted report, got " <> show desync),
      HU.testCase "a mismatched entry is reported, not silently defaulted" $ do
        (start, game, recorded, transcript) <- recordedGame registry
        -- A response of the wrong SHAPE for the first prompt, which is the
        -- opening Prompt.Shuffle: Response.ChoseX answers Prompt.ChooseX and
        -- nothing else, so decode rejects it.
        let bogus = Response.ChoseX 0
            ((_, replayed), desync) = Replay.replay (bogus : transcript) start game
        HU.assertEqual "reported at index 0, carrying the offending entry" (Just (Desync.Mismatched 0 bogus)) desync
        HU.assertBool "and the replay is not the recorded game" (recorded /= replayed),
      -- The one that matters: a dropped concession replays to the OTHER winner.
      HU.testCase "#144 a concession lost to a desync silently flips the winner" $
        let base = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
            ((_, conceded), transcript) = Replay.record concedeAnswer base Engine.runStep
            -- One spurious entry ahead of the log is enough: decode rejects it,
            -- replay does not consume it, and every later prompt meets the same
            -- entry -- so the whole transcript is stranded behind it.
            ((_, drifted), desync) = Replay.replay (Response.ChoseX 0 : transcript) base Engine.runStep
         in do
              HU.assertEqual "recorded: alice conceded, bob wins" (Just (Result.Won S.bob)) (GameState.result conceded)
              HU.assertEqual "replayed: the concession is gone" Nothing (GameState.result drifted)
              HU.assertEqual "and the report says so" (Just (Desync.Mismatched 0 (Response.ChoseX 0))) desync,
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
            let legal = Target.fillableModes Nothing acId Map.empty (TriggeredAbility.modal ability) gs
                p = Prompt.ChooseModes decider S.alice acId legal 1
                answer = Set.singleton (ModeIndex.MkModeIndex 2)
            HU.assertEqual "legal modes are 0 and 2 (bounce self-excluded)" (Set.fromList [ModeIndex.MkModeIndex 0, ModeIndex.MkModeIndex 2]) legal
            HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer))
          _ -> HU.assertFailure "Aether Channeler must have exactly one triggered ability"
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Replay" [replayTests registry, combatReplayTests]
