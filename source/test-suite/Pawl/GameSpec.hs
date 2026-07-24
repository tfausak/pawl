{-# LANGUAGE GADTs #-}

-- Covers Pawl.Game, Pawl.Engine, and Pawl.Action: zones and changeZone, legal
-- actions, object facts, engine steps, and engine-rule integration (priority
-- rounds, the CR 103.7a draw skip, CR 514.2 discard, CR 704.5b deck-out).
module Pawl.GameSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Cast as Cast
import qualified Pawl.Cost as Cost
import qualified Pawl.Decide as Decide
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Concession as Concession
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Timestamp as Timestamp
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

objectFactTests :: Cards.Cards -> Tasty.TestTree
objectFactTests cards =
  Tasty.testGroup
    "ObjectFacts"
    [ HU.testCase "a Piker's power and toughness are 2 and 1" $
        let (oid, gs) = S.addPiker cards S.alice (Setup.emptyGame S.bothPlayers)
         in do
              HU.assertEqual "power" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs),
      HU.testCase "CR 111.3 a token's characteristics are read through cardOf" $
        let base = Setup.emptyGame S.bothPlayers
            goblinCard = Printing.card (Cards.pikerPrinting cards)
            (tokId, gs) = S.addToken goblinCard S.alice base
         in do
              HU.assertEqual "cardOf returns the token's embedded card" (Just goblinCard) (Game.cardOf tokId gs)
              HU.assertEqual "the token is on the battlefield" True (Set.member tokId (GameState.battlefield gs)),
      HU.testCase "CR 112.1 isSpell is True for a spell on the stack, False off it" $
        let base = Setup.emptyGame S.bothPlayers
            (spellId, gs1) = S.spellOnStack (Cards.pikerPrinting cards) S.alice base
            (permId, gs2) = S.addPiker cards S.bob gs1
            tokenCard = Printing.card (Cards.pikerPrinting cards)
            (tokId, gs3) = S.addToken tokenCard S.bob gs2
         in do
              HU.assertBool "a card on the stack is a spell" (Game.isSpell spellId gs3)
              HU.assertBool "a battlefield permanent is not a spell" (not (Game.isSpell permId gs3))
              HU.assertBool "a token is not a spell" (not (Game.isSpell tokId gs3)),
      HU.testCase "a Mountain has no power or toughness" $
        let gs = S.mountainsInPlay cards 1
         in case Game.zoneMembers Zone.Battlefield S.alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> do
                HU.assertEqual "power" Nothing (Projection.powerOf oid gs)
                HU.assertEqual "toughness" Nothing (Projection.toughnessOf oid gs),
      HU.testCase "controllerOf is the owner while nothing can change control" $
        let (oid, gs) = S.addPiker cards S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "controller" (Just S.bob) (Projection.controllerOf oid gs),
      HU.testCase "an unknown id has no facts" $
        let gs = Setup.emptyGame S.bothPlayers
            missing = ObjectId.MkObjectId 999
         in do
              HU.assertEqual "power" Nothing (Projection.powerOf missing gs)
              HU.assertEqual "controller" Nothing (Projection.controllerOf missing gs)
    ]

gameTests :: Cards.Cards -> Tasty.TestTree
gameTests cards =
  let after = S.runPure S.identityAnswer (S.oneMountainState cards Phase.PrecombatMain) (Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield)
   in Tasty.testGroup
        "Game"
        [ HU.testCase "changeZone preserves object count" $
            HU.assertEqual "count" 1 (Game.objectCount after),
          HU.testCase "changeZone drops the old id" $
            HU.assertEqual "old gone" Nothing (Game.lookupObject (ObjectId.MkObjectId 0) after),
          HU.testCase "the moved object is on the battlefield, owner preserved" $
            HU.assertEqual
              "moved"
              ( Just
                  Object.MkObject
                    { Object.owner = S.alice,
                      Object.source = Source.OfCard (Cards.mountainPrinting cards),
                      Object.zone = Zone.Battlefield,
                      Object.tapped = TapState.Untapped,
                      Object.damage = 0,
                      Object.sickness = Sickness.Sick,
                      Object.bindings = Map.empty,
                      Object.counters = Map.empty,
                      -- changeZone draws a fresh timestamp; oneMountainState's
                      -- nextTimestamp starts at 1 (object 0 already holds 0).
                      Object.timestamp = Timestamp.MkTimestamp 1
                    }
              )
              (Game.lookupObject (ObjectId.MkObjectId 1) after),
          HU.testCase "CR 400.7 changeZone forgets a spell's bindings" $
            let base = S.oneMountainState cards Phase.PrecombatMain
                slot = SlotName.MkSlotName (Text.pack "target")
                stamped =
                  base
                    { GameState.objects =
                        Map.adjust
                          (\o -> o {Object.bindings = Binding.fromChoices (Map.singleton slot (Recipient.ToPlayer S.alice)) Map.empty Nothing Set.empty})
                          (ObjectId.MkObjectId 0)
                          (GameState.objects base)
                    }
                moved = S.runPure S.identityAnswer stamped (Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield)
                landed = Map.elems (Map.filter (\o -> Object.zone o == Zone.Battlefield) (GameState.objects moved))
             in HU.assertEqual "reset" [Map.empty] (map Object.bindings landed),
          HU.testCase "CR 400.7 changeZone resets a word-swap binding" $
            let base = S.oneMountainState cards Phase.PrecombatMain
                slot = SlotName.MkSlotName (Text.pack "target")
                stamped =
                  base
                    { GameState.objects =
                        Map.adjust
                          (\o -> o {Object.bindings = Binding.fromChoices Map.empty (Map.singleton slot (Subtype.Mountain, Subtype.Island)) Nothing Set.empty})
                          (ObjectId.MkObjectId 0)
                          (GameState.objects base)
                    }
                moved = S.runPure S.identityAnswer stamped (Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield)
                newObj = Game.lookupObject (ObjectId.MkObjectId 1) moved
             in HU.assertEqual "reset to empty" (Just Map.empty) (fmap Object.bindings newObj),
          HU.testCase "CR 613.7d changeZone stamps the new incarnation with a fresh timestamp" $
            let (oid, gs) = S.addPiker cards S.bob (S.mountainsInPlay cards 1)
                before = GameState.nextTimestamp gs
                movedState = S.runPure S.identityAnswer gs (Event.changeZone oid Zone.Graveyard)
                movedId = case Game.zoneMembers Zone.Graveyard S.bob movedState of
                  i : _ -> i
                  [] -> ObjectId.MkObjectId 999
                stamp = fmap Object.timestamp (Game.lookupObject movedId movedState)
             in do
                  HU.assertEqual "the incarnation carries the pre-move next timestamp" (Just before) stamp
                  HU.assertBool "the counter advanced" (GameState.nextTimestamp movedState > before),
          HU.testCase "emptyGame starts the timestamp counter at zero" $
            HU.assertEqual "zero" (Timestamp.MkTimestamp 0) (GameState.nextTimestamp (Setup.emptyGame S.bothPlayers)),
          HU.testCase "a fresh game has no continuous effects" $
            HU.assertEqual "empty" [] (GameState.continuousEffects (Setup.emptyGame S.bothPlayers)),
          HU.testCase "a vanilla printing declares no static abilities" $
            HU.assertEqual "empty" [] (Card.Type.staticAbilities (S.pikerCard cards))
        ]

actionTests :: Cards.Cards -> Tasty.TestTree
actionTests cards =
  Tasty.testGroup
    "Action"
    [ HU.testCase "a land in hand is playable in a main phase" $
        HU.assertBool "play" (A.Play (ObjectId.MkObjectId 0) `elem` Action.legalActions S.alice (S.oneMountainState cards Phase.PrecombatMain)),
      HU.testCase "passing is always legal" $
        HU.assertBool "pass" (A.Pass `elem` Action.legalActions S.alice (S.oneMountainState cards Phase.PrecombatMain)),
      HU.testCase "no land play outside a main phase" $
        HU.assertEqual "only pass" [A.Pass] (Action.legalActions S.alice (S.oneMountainState cards (Phase.Beginning BeginningStep.Upkeep))),
      HU.testCase "no second land after one is played" $
        let gs = (S.oneMountainState cards Phase.PrecombatMain) {GameState.landPlayed = Set.singleton S.alice}
         in HU.assertEqual "only pass" [A.Pass] (Action.legalActions S.alice gs)
    ]

goldfishResult :: Cards.Cards -> (Result.Result, GameState.GameState)
goldfishResult cards =
  Engine.runMatchPure S.identityAnswer (S.redRed cards)

landState :: Cards.Cards -> GameState.GameState
landState cards =
  snd (Engine.runGamePure S.playLandAnswer (Setup.emptyGame S.bothPlayers) (Engine.playFrom (S.redRed cards)))

-- Alice is active on turns 1, 3, 5, …; bob on 2, 4, 6, …. With one land play per
-- turn (CR 305.2) a player can never have more lands out than turns taken.
turnsTaken :: PlayerId.PlayerId -> GameState.GameState -> Int
turnsTaken pid gs =
  let total = fromIntegral (GameState.turnNumber gs)
   in if pid == S.alice then (total + 1) `div` 2 else total `div` 2

engineTests :: Cards.Cards -> Tasty.TestTree
engineTests cards =
  Tasty.testGroup
    "Engine"
    [ HU.testCase "goldfish game ends with the starting player winning" $
        HU.assertEqual "winner" (Result.Won S.alice) (fst (goldfishResult cards)),
      HU.testCase "card conservation holds at end" $
        HU.assertEqual "objects" 120 (Game.objectCount (snd (goldfishResult cards))),
      HU.testCase "playing lands fills the battlefield" $
        HU.assertBool "non-empty" $
          not (null (Game.zoneMembers Zone.Battlefield S.alice (landState cards))),
      HU.testCase "land play conserves cards" $
        HU.assertEqual "objects" 120 (Game.objectCount (landState cards)),
      HU.testCase "CR 305.2 at most one land per turn" $
        HU.assertBool "no double land plays" $
          length (Game.zoneMembers Zone.Battlefield S.alice (landState cards)) <= turnsTaken S.alice (landState cards)
            && length (Game.zoneMembers Zone.Battlefield S.bob (landState cards)) <= turnsTaken S.bob (landState cards)
    ]

-- Run setup, then a scripted tweak, then whatever steps the (scenario cards) needs.
scenario :: Cards.Cards -> Game.Type.Game () -> GameState.GameState
scenario cards steps =
  snd $ Engine.runGamePure S.identityAnswer (Setup.emptyGame (NonEmpty.map fst (S.redRed cards))) $ do
    Setup.newGame (S.redRed cards)
    steps

-- Alice starts, so her turn-1 draw is skipped.
aliceFirstDraw :: Cards.Cards -> GameState.GameState
aliceFirstDraw cards = scenario cards S.drawStep

-- Bob is not the starting player, so his draw happens normally.
bobFirstDraw :: Cards.Cards -> GameState.GameState
bobFirstDraw cards = scenario cards $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
  S.drawStep

bobAfterCleanup :: Cards.Cards -> GameState.GameState
bobAfterCleanup cards = scenario cards $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
  S.drawStep
  Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)

deckedOut :: Cards.Cards -> GameState.GameState
deckedOut cards = scenario cards $ do
  State.modify' $ \gs ->
    gs
      { GameState.library = Map.insert S.alice Seq.empty (GameState.library gs),
        GameState.turnNumber = 3
      }
  S.drawStep
  Engine.checkSba

librarySize :: PlayerId.PlayerId -> GameState.GameState -> Int
librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)

-- Records every player asked for an action, in order, and casts when it can.
-- Recording is the point: whether the caster RETAINS priority is only visible in
-- who gets asked next.
recordingAnswer :: Prompt.Prompt r -> State.State [PlayerId.PlayerId] r
recordingAnswer p = case p of
  Prompt.Concede _ -> pure Concession.Continues
  Prompt.DeclareAttackers {} -> pure []
  Prompt.DeclareBlockers {} -> pure Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    pure $ case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.Shuffle ids -> pure ids
  Prompt.RandomFirstPlayer order -> pure (NonEmpty.head order)
  Prompt.ChooseTargets _ _ _ sets -> pure (Map.mapMaybe Set.lookupMin sets)
  Prompt.ChooseDiscard _ _ ids n -> pure (take (fromIntegral n) ids)
  Prompt.ChooseAction _ pid actions -> do
    State.modify' (\asked -> asked ++ [pid])
    let isCast a = case a of
          A.Cast _ -> True
          _ -> False
    pure $ case filter isCast actions of
      h : _ -> h
      [] -> A.Pass
  Prompt.ChooseBasicLandTypes {} -> pure (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> pure Nothing
  Prompt.CastWhileSearching {} -> pure Nothing
  Prompt.ChooseX {} -> pure 0
  Prompt.ChooseModes _ _ _ legal count -> pure (Set.fromList (take (fromIntegral count) (Set.toAscList legal)))
  Prompt.ChooseCopyTarget {} -> pure Nothing
  Prompt.ChooseEntryOption {} -> pure 0
  Prompt.OrderTriggers _ _ sources -> pure (map fromIntegral (take (length sources) [0 :: Int ..]))
  Prompt.ChooseReplacement {} -> pure 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> pure (Set.fromList (take (fromIntegral count) candidates))
  Prompt.ChooseCost _ _ _ candidates -> pure (Cost.firstOffered candidates)
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> pure (take (fromIntegral count) hand)

-- pikerInHand already builds on Setup.emptyGame bothPlayers, so turnOrder is
-- [alice, bob] and both players are in the players map.
askedPlayers :: Cards.Cards -> [PlayerId.PlayerId]
askedPlayers cards =
  let (gs, _) = S.pikerInHand cards 3 Phase.PrecombatMain
   in State.execState
        (Program.foldProgramM recordingAnswer (State.runStateT Engine.priorityLoop gs))
        []

ruleTests :: Cards.Cards -> Tasty.TestTree
ruleTests cards =
  Tasty.testGroup
    "Rules"
    [ HU.testCase "CR 117.4 a full round of passes resolves the stack, not the step" $
        -- With a spell on the stack, everyone passing must RESOLVE it and keep
        -- the step alive. Under M0's rule the step would simply end with the
        -- spell still sitting on the stack.
        let (gs, oid) = S.pikerInHand cards 3 Phase.PrecombatMain
            steps = do
              Cast.castSpell S.alice oid
              Engine.priorityLoop
            after = snd (Engine.runGamePure S.identityAnswer gs steps)
         in do
              HU.assertEqual "stack emptied" 0 (length (GameState.stack after))
              HU.assertEqual "piker resolved onto the battlefield" 1 (S.creaturesInPlay S.alice after),
      HU.testCase "CR 117.3c the caster is asked again, rather than passing priority on" $
        -- alice is asked, casts, and must be asked AGAIN before bob gets a turn.
        -- If priority wrongly advanced to the next player, this would be
        -- [alice, bob, ...] instead.
        HU.assertEqual "alice twice, then bob" [S.alice, S.alice, S.bob] (take 3 (askedPlayers cards)),
      HU.testCase "CR 103.7a starting player skips first draw" $ do
        HU.assertEqual "hand" 7 (S.handSize S.alice (aliceFirstDraw cards))
        HU.assertEqual "library" 53 (librarySize S.alice (aliceFirstDraw cards)),
      HU.testCase "CR 103.7a only the starting player skips" $ do
        HU.assertEqual "hand" 8 (S.handSize S.bob (bobFirstDraw cards))
        HU.assertEqual "library" 52 (librarySize S.bob (bobFirstDraw cards)),
      HU.testCase "CR 514.2 discard to hand size" $
        HU.assertEqual "hand" 7 (S.handSize S.bob (bobAfterCleanup cards)),
      HU.testCase "CR 704.5b deck-out loses" $
        HU.assertEqual
          "alice departed"
          (Just (Status.Departed Departure.Lost))
          (fmap Player.status (Map.lookup S.alice (GameState.players (deckedOut cards)))),
      HU.testCase "CR 704.5b the survivor wins" $
        HU.assertEqual "bob won" (Just (Result.Won S.bob)) (GameState.result (deckedOut cards)),
      HU.testCase "CR 723.3/723.5: alice decides for bob, but bob's resources move" $
        -- bob's main phase, controlled by alice, with a Mountain and a Bolt.
        let g0 = Setup.emptyGame S.bothPlayers
            (_mtnId, g1) = S.addCreature (Cards.mountainPrinting cards) S.bob g0
            (_boltId, g2) = handBobBolt cards g1
            g3 =
              g2
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = snd (Engine.runGamePure slaveAnswer g3 Engine.priorityLoop)
            boltInBobGrave =
              length
                ( filter
                    (namedIs (Text.pack "Lightning Bolt"))
                    (map (\i -> Game.lookupObject i after) (Game.zoneMembers Zone.Graveyard S.bob after))
                )
         in do
              HU.assertEqual "bob took 3 from his own Bolt" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "bob's Bolt is in bob's graveyard" 1 boltInBobGrave
              HU.assertEqual "the Mountain (bob's) is tapped" 1 (S.tappedCount S.bob after),
      HU.testCase "CR 723.1/723.3 gameplay: Mindslaver hands alice bob's whole turn, then control lapses" $
        -- Alice activates a REAL Mindslaver through the driver loop at bob, the
        -- engine promotes control on bob's turn, alice casts bob's Bolt at bob
        -- (bob's own resource), and control ends at the following turn boundary.
        let g0 = Setup.emptyGame S.bothPlayers
            (_msId, g1) = S.addCreature (Cards.mindslaverPrinting cards) S.alice g0
            -- {4} for Mindslaver's activation: four untapped Mountains for alice.
            (_a1, g2) = S.addCreature (Cards.mountainPrinting cards) S.alice g1
            (_a2, g3) = S.addCreature (Cards.mountainPrinting cards) S.alice g2
            (_a3, g4) = S.addCreature (Cards.mountainPrinting cards) S.alice g3
            (_a4, g5) = S.addCreature (Cards.mountainPrinting cards) S.alice g4
            -- bob's own resources for his controlled turn: a Mountain and a Bolt.
            (_bMtn, g6) = S.addCreature (Cards.mountainPrinting cards) S.bob g5
            (_bBolt, g7) = handBobBolt cards g6
            gStart =
              g7
                { GameState.activePlayer = S.alice,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
            -- Alice's turn: activate Mindslaver at bob; the ability resolves and
            -- installs pending control for bob (CR 723.1).
            afterActivation = snd (Engine.runGamePure gateAnswer gStart Engine.priorityLoop)
            -- Handoff to bob's turn promotes pendingControl -> activeControl.
            bobsTurn = snd (Engine.runGamePure gateAnswer afterActivation Engine.handoffTurn)
            -- Bob's controlled main phase: alice decides, casting bob's Bolt at bob.
            bobMain = bobsTurn {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.bob}
            bobPlayed = snd (Engine.runGamePure gateAnswer bobMain Engine.priorityLoop)
            -- Next handoff (bob -> alice) clears control (CR 723.1: ends at the
            -- beginning of the next turn).
            afterBob = snd (Engine.runGamePure gateAnswer bobPlayed Engine.handoffTurn)
            boltInBobGrave =
              length
                ( filter
                    (namedIs (Text.pack "Lightning Bolt"))
                    (map (\i -> Game.lookupObject i bobPlayed) (Game.zoneMembers Zone.Graveyard S.bob bobPlayed))
                )
         in do
              HU.assertEqual "CR 723.1: control pending for bob after activation" (Just (Decider.MkDecider S.alice)) (Map.lookup S.bob (GameState.pendingControl afterActivation))
              HU.assertEqual "CR 723.1: promoted to active control on bob's turn" (Just (Decider.MkDecider S.alice)) (GameState.activeControl bobsTurn)
              HU.assertEqual "CR 723.3: bob is still the active player while controlled" S.bob (GameState.activePlayer bobsTurn)
              HU.assertEqual "CR 723.5: bob's decisions route to alice" (Decider.MkDecider S.alice) (Decide.deciderFor S.bob bobsTurn)
              HU.assertEqual "alice's whole-turn choice moved bob's life" (Just 17) (S.lifeOf S.bob bobPlayed)
              HU.assertEqual "bob's Bolt went to bob's graveyard" 1 boltInBobGrave
              HU.assertEqual "bob's Mountain (his resource) is tapped" 1 (S.tappedCount S.bob bobPlayed)
              HU.assertEqual "CR 723.1: control lapses at the next turn" (Decider.MkDecider S.bob) (Decide.deciderFor S.bob afterBob)
              HU.assertEqual "active control cleared after bob's turn" Nothing (GameState.activeControl afterBob),
      HU.testCase "CR 723.5 combat: alice declares bob's attackers, so alice takes the hit" $
        -- bob's turn, controlled by alice, with one 2/1 Piker. combatBoardOf sets
        -- alice active with `mine` and bob with `theirs`; here alice attacks with
        -- nothing and bob has the Piker, and we flip the active player to bob.
        let (board, _mine, _bobsPikers) = S.combatBoardOf [] [Cards.pikerPrinting cards]
            g0 =
              board
                { GameState.activePlayer = S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = S.runCombat controlCombatAnswer g0
         in HU.assertEqual "alice took 2 from bob's Piker, declared by alice-as-bob" (Just 18) (S.lifeOf S.alice after),
      HU.testCase "CR 723.5a: the controller spends only the controlled player's resources" $
        -- bob (controlled by alice) and alice each have an untapped Mountain; bob
        -- has a Bolt. Alice-as-bob casts bob's Bolt, paid from BOB's Mountain.
        -- alice's Mountain and hand must be untouched.
        let g0 = Setup.emptyGame S.bothPlayers
            (_bMtn, g1) = S.addCreature (Cards.mountainPrinting cards) S.bob g0
            (_aMtn, g2) = S.addCreature (Cards.mountainPrinting cards) S.alice g1
            (_bBolt, g3) = handBobBolt cards g2
            g4 =
              g3
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.bob,
                  GameState.activeControl = Just (Decider.MkDecider S.alice)
                }
            after = snd (Engine.runGamePure slaveAnswer g4 Engine.priorityLoop)
         in do
              HU.assertEqual "bob took 3 from his own Bolt" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "bob's Mountain (his resource) is tapped" 1 (S.tappedCount S.bob after)
              HU.assertEqual "CR 723.5a: alice's Mountain is untouched" 0 (S.tappedCount S.alice after)
              HU.assertEqual "CR 723.5a: alice's hand is untouched" 0 (S.handSize S.alice after),
      HU.testCase "CR 727.1/727.2/727.4 gameplay: bob activates a restart and the game rebuilds from its own cards" $
        -- bob controls the synthetic restart artifact and owns 8 cards total;
        -- alice owns 8. Both start with reduced life on a populated board. bob
        -- activates the artifact through the priority loop; it resolves, restarts
        -- the game, and the result is a valid new game with bob as starter.
        let g0 = Setup.emptyGame S.bothPlayers
            (_restartId, g1) = S.addCreature (Cards.syntheticRestartPrinting cards) S.bob g0
            (_aPiker, g2) = S.addCreature (Cards.pikerPrinting cards) S.alice g1
            -- fill each owner's pool to >= 7 cards so opening hands draw without a
            -- CR 727.3 loss (the restart artifact + 7 mountains = 8 for bob).
            g3 = addManyG cards 7 S.bob (addManyG cards 7 S.alice g2)
            gStart =
              g3
                { GameState.activePlayer = S.bob,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.bob,
                  -- Knock both players to 8 life so "both players reset to 20
                  -- life" below is load-bearing: Setup.emptyGame already starts
                  -- players at 20, so without this reduction the assertions
                  -- would pass even if resetPlayer did nothing.
                  GameState.players = Map.adjust (\p -> p {Player.life = 8}) S.alice (Map.adjust (\p -> p {Player.life = 8}) S.bob (GameState.players g3))
                }
            after = snd (Engine.runGamePure restartAnswer gStart Engine.priorityLoop)
         in do
              HU.assertEqual "CR 727.1a: bob is the new active player" S.bob (GameState.activePlayer after)
              HU.assertEqual "CR 727.1a: the turn order begins with bob" (Just S.bob) (Maybe.listToMaybe (GameState.turnOrder after))
              HU.assertEqual "both players reset to 20 life (alice)" (Just 20) (S.lifeOf S.alice after)
              HU.assertEqual "both players reset to 20 life (bob)" (Just 20) (S.lifeOf S.bob after)
              HU.assertEqual "CR 103.5: alice drew a 7-card opening hand" 7 (S.handSize S.alice after)
              HU.assertEqual "CR 103.5: bob drew a 7-card opening hand" 7 (S.handSize S.bob after)
              HU.assertEqual "CR 727.4: settled at the first untap step" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "CR 727.2: the battlefield is empty (every card returned to a library)" True (Set.null (GameState.battlefield after))
              HU.assertEqual "the game did not end -- the new game is live" Nothing (GameState.result after),
      HU.testCase "CR 729.2/729.3/729.5: playSubgame runs a nested game, bob decks, cards funnel back" $
        let g0 = Setup.emptyGame S.bothPlayers
            -- alice: 8 library cards; bob: 3 (fewer than seven -> loses, CR 729.3)
            g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 3 S.bob (addManyG cards 8 S.alice g0)))
            (result, after) = Engine.runGamePure S.identityAnswer g1 Engine.playSubgame
            libCount pid = length (Game.zoneMembers Zone.Library pid after)
         in do
              HU.assertEqual "CR 729.3: bob has fewer than 7 cards, so alice wins the subgame" (Result.Won S.alice) result
              HU.assertEqual "CR 729.5: alice's cards funnel back into her main-game library" 8 (libCount S.alice)
              HU.assertEqual "CR 729.5: bob's cards funnel back into his main-game library" 3 (libCount S.bob)
              HU.assertEqual "the main game resumes with no result recorded" Nothing (GameState.result after),
      -- #136 / CR 729.2: "Randomly determine which player goes first." The
      -- interpreter supplies the randomness (Prompt.RandomFirstPlayer); the
      -- engine only asks. Both players get libraries of EXACTLY seven, so each
      -- opening hand (CR 103.5) empties its library without drawing from empty:
      -- nobody loses during setup. The starting player then skips their first
      -- draw (CR 103.7a), so the OTHER player is the one who draws from an empty
      -- library on turn 2 and decks (CR 704.5b) -- the subgame's winner is
      -- exactly whoever the roll started. Flipping the answer flips the winner,
      -- which is what makes the determination observable rather than cosmetic.
      HU.testCase "CR 729.2: the subgame's first player comes from the roll; the answer decides who wins" $
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 7 S.bob (addManyG cards 7 S.alice g0)))
            winnerWhenStarting starter = fst (Engine.runGamePure (firstPlayerAnswer starter) g1 Engine.playSubgame)
         in do
              HU.assertEqual "alice starts, skips her first draw, and bob decks on turn 2" (Result.Won S.alice) (winnerWhenStarting S.alice)
              HU.assertEqual "bob starts, skips his first draw, and alice decks on turn 2" (Result.Won S.bob) (winnerWhenStarting S.bob),
      HU.testCase "CR 729.2: a lone player is not asked -- the determination is forced" $
        -- Where the rules leave nothing to determine, don't prompt: with one
        -- player in the turn order, every roll yields the same starter. alice's
        -- library is empty, so her opening draw decks her (CR 704.5b) and the
        -- subgame ends during setup -- long enough to record an ask if one were
        -- made, which is what the transcript is inspected for.
        let g0 = Setup.emptyGame (S.alice NonEmpty.:| [])
            (_, log_) = Replay.record S.identityAnswer g0 Engine.playSubgame
            isRoll r = case r of
              Response.DeterminedFirstPlayer _ -> True
              _ -> False
         in HU.assertEqual "no first-player roll was recorded" 0 (length (filter isRoll log_)),
      HU.testCase "CR 103.1/729.2: the subgame's turn order is rotated to begin with the starting player" $
        -- Not just activePlayer: Engine.skipsDraw (CR 103.7a) reads the HEAD of
        -- the turn order, so a subgame that set one without the other would hand
        -- the skip to the wrong player.
        let sub = Setup.subgameStateFrom S.bob (Setup.emptyGame S.bothPlayers)
         in do
              HU.assertEqual "bob is the subgame's active player" S.bob (GameState.activePlayer sub)
              HU.assertEqual "the subgame turn order begins with bob" [S.bob, S.alice] (GameState.turnOrder sub),
      HU.testCase "CR 729.1b/729.3 gameplay: alice casts a subgame spell, bob decks, bob takes 3" $
        -- alice has the {0} subgame sorcery in hand and an 8-card library; bob has
        -- a 3-card library (decks in the subgame, CR 729.3). alice casts through
        -- the priority loop; the subgame resolves alice the winner; the follow-on
        -- DealDamage hits bob (the loser) for 3.
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 3 S.bob (addManyG cards 8 S.alice g0)))
            (spellId, g2) = S.addHandCard (Cards.syntheticSubgamePrinting cards) S.alice g1
            gStart =
              g2
                { GameState.activePlayer = S.alice,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
            after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
         in do
              HU.assertEqual "CR 729.1b: bob (the subgame loser) took 3 from the follow-on DealDamage" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "alice, the winner, is untouched" (Just 20) (S.lifeOf S.alice after)
              HU.assertEqual "the subgame spell resolved and left the stack" [] (GameState.stack after)
              HU.assertEqual "the main game did not end" Nothing (GameState.result after)
              -- Casting routes through changeZone (CR 400.7), which mints a fresh
              -- id and drops spellId entirely -- Game.lookupObject spellId after
              -- is unconditionally Nothing, so that alone proves nothing about
              -- alice's hand. The load-bearing check is that spellId's old
              -- incarnation is not lingering in her hand's member list.
              HU.assertEqual "the subgame spell's original id no longer sits in alice's hand (cast)" True (notElem spellId (Game.zoneMembers Zone.Hand S.alice after)),
      HU.testCase "CR 729.5/729.4b gameplay: cards funnel back, main-game board survives, main-game counters untouched" $
        -- library pool built first, THEN the survivor is added to the battlefield --
        -- poolToLibraryG sweeps every object a player owns onto their library, so a
        -- survivor added before it would be swept in too and vanish from the board.
        let g0 = Setup.emptyGame S.bothPlayers
            g1 = poolToLibraryG S.bob (poolToLibraryG S.alice (addManyG cards 3 S.bob (addManyG cards 8 S.alice g0)))
            -- a survivor on the main battlefield that must remain after the subgame
            (survivorId, g2) = S.addCreature (Cards.mountainPrinting cards) S.alice g1
            (_spellId, g3) = S.addHandCard (Cards.syntheticSubgamePrinting cards) S.alice g2
            -- give bob a main-game poison counter (CR 729.4b: outside the subgame)
            g4 =
              g3
                { GameState.players =
                    Map.adjust
                      (\pl -> pl {Player.counters = Map.insert PlayerCounterKind.Poison 1 (Player.counters pl)})
                      S.bob
                      (GameState.players g3)
                }
            gStart =
              g4
                { GameState.activePlayer = S.alice,
                  GameState.phase = Phase.PrecombatMain,
                  GameState.priority = Just S.alice
                }
            after = snd (Engine.runGamePure subgameAnswer gStart Engine.priorityLoop)
            bobPoison =
              maybe
                0
                (Map.findWithDefault 0 PlayerCounterKind.Poison . Player.counters)
                (Map.lookup S.bob (GameState.players after))
         in do
              HU.assertEqual "CR 729.4b: bob's main-game poison counter is untouched by the subgame" 1 bobPoison
              HU.assertEqual "CR 729.5: alice's library holds her 8 subgame cards again" 8 (length (Game.zoneMembers Zone.Library S.alice after))
              HU.assertEqual "CR 729.5: bob's library holds his 3 subgame cards again" 3 (length (Game.zoneMembers Zone.Library S.bob after))
              HU.assertEqual "the main-game survivor is still on the battlefield" True (Set.member survivorId (GameState.battlefield after)),
      HU.testCase "CR 729.6 gameplay: a subgame nests a subgame; nesting terminates and the main game resumes" $
        -- alice's MAIN-GAME library feeds level 1's library: one nested
        -- synthetic-subgame sorcery + 13 Mountains (14 total). The level-1 opening
        -- hand draws 7 (the sorcery + 6 Mountains), leaving exactly 7 Mountains in
        -- her level-1 library -- enough that she does NOT deck when level 2's
        -- opening hand draws from it (CR 729.2 pulls a subgame's library from its
        -- parent's library). bob's library is exactly 7 Mountains: his level-1
        -- opening hand consumes all seven (no immediate CR 704.5b loss -- every
        -- draw still succeeds), leaving his level-1 library EMPTY, so he decks at
        -- his own level-1 draw step (turn 2) -- real level-1 play, not an instant
        -- SBA loss before anyone gets priority. Sized this way (rather than the
        -- brief's flat "3 Mountains decks instantly") because an immediate deck-out
        -- during subgame setup fires before ANY priorityLoop grants alice priority
        -- (Sba.losesNow reads GameState.drewFromEmpty, set during the opening-hand
        -- draw itself), which would leave alice no window to cast the nested
        -- sorcery at all and collapse this gate to a flat (non-nested) subgame.
        --
        -- CR 729.1a's isolation means a subgame's INTERNAL choices leave no trace
        -- in the parent's GameState -- but the interpreter TRANSCRIPT (every
        -- Response, recorded by Pawl.Replay.record) is a top-level observable, and
        -- it DOES discriminate nesting depth: each subgame level's setup
        -- (subgameStateFrom -> startGameFromCards) shuffles every player's
        -- library once, and playSubgame's CR 729.5 funnel-back reshuffles the
        -- parent's library once per player -- 2 Response.Shuffled entries per
        -- level, per funnel. A flat (single-level) subgame gate contributes 4
        -- (2 setup + 2 funnel-back); this two-level gate contributes 8 (2 levels
        -- x (2 setup + 2 funnel-back)), so asserting the count is a genuine
        -- nesting regression test, not just a termination guard.
        let g0 = Setup.emptyGame S.bothPlayers
            (_nestedId, g1) = libraryCard (Cards.syntheticSubgamePrinting cards) S.alice g0
            g2 = poolToLibraryG S.bob (addToLibraryG cards 13 S.alice (addManyG cards 7 S.bob g1))
            (_spellId, g3) = S.addHandCard (Cards.syntheticSubgamePrinting cards) S.alice g2
            gStart = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
            ((_, after), log_) = Replay.record subgameAnswer gStart Engine.priorityLoop
            isShuffled r = case r of
              Response.Shuffled _ -> True
              _ -> False
            shuffles = length (filter isShuffled log_)
         in do
              -- If nesting had not terminated, runGamePure/Replay.record would not return.
              HU.assertEqual "CR 729.6: the top-level main game resumed with no result" Nothing (GameState.result after)
              HU.assertEqual "CR 729.1b: bob took 3 from the level-1 subgame's follow-on" (Just 17) (S.lifeOf S.bob after)
              HU.assertEqual "the top-level subgame spell left the stack" [] (GameState.stack after)
              HU.assertEqual "CR 729.6: two nested subgame levels each shuffle on setup and funnel-back (measured; a flat gate yields 4)" 8 shuffles,
      HU.testCase "a subgame replays deterministically (the reason Prompt.PlaySubgame was rejected, CR 729 / M0's determinism criterion)" $
        -- A Prompt would run the subgame INSIDE the answer function, below
        -- Replay.record's interposition point, so its inner choices could never
        -- be recovered from the recorded transcript. Round-tripping this nested
        -- gate's fixture through record -> replay and comparing the final
        -- GameState (derives Eq) is the test that would fail if that design had
        -- been taken instead.
        let g0 = Setup.emptyGame S.bothPlayers
            (_nestedId, g1) = libraryCard (Cards.syntheticSubgamePrinting cards) S.alice g0
            g2 = poolToLibraryG S.bob (addToLibraryG cards 13 S.alice (addManyG cards 7 S.bob g1))
            (_spellId, g3) = S.addHandCard (Cards.syntheticSubgamePrinting cards) S.alice g2
            gStart = g3 {GameState.activePlayer = S.alice, GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice}
            ((_, after), log_) = Replay.record subgameAnswer gStart Engine.priorityLoop
            (_, replayed) = Replay.replay log_ gStart Engine.priorityLoop
         in HU.assertEqual "a subgame replays deterministically (the reason PlaySubgame is not a Prompt, CR 729 / M0 determinism)" after replayed
    ]

-- #133 / CR 104.3a. Concede is a special action, not a card, so the gate is
-- gameplay-level. The central case is CR 723.6: a Mindslaver controller may not
-- concede for the player they control, but that player may still concede
-- themselves -- which is why Prompt.Concede carries no Decider.

-- Concedes for exactly one player, continues for everyone else, and otherwise
-- passes. The PlayerId the prompt carries is the TRUE player: if the engine ever
-- routed this through Decide.deciderFor, this answerer would concede for the
-- wrong person and the CR 723.6 test below would fail.
concedeAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
concedeAnswer who p = case p of
  Prompt.Concede asked -> if asked == who then Concession.Concedes else Concession.Continues
  _ -> S.identityAnswer p

concedeTests :: Cards.Cards -> Tasty.TestTree
concedeTests cards =
  Tasty.testGroup
    "concede (CR 104.3a)"
    [ HU.testCase "CR 104.3a/104.2a a concede ends the game immediately, opponent wins" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
            after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
         in HU.assertEqual "bob wins" (Just (Result.Won S.bob)) (GameState.result after),
      HU.testCase "the conceding player departs as Conceded, not Lost" $
        let gs = (Setup.emptyGame S.bothPlayers) {GameState.phase = Phase.PrecombatMain, GameState.activePlayer = S.alice}
            after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
         in HU.assertEqual "reason recorded" (Just (Status.Departed Departure.Conceded)) (fmap Player.status (Map.lookup S.alice (GameState.players after))),
      HU.testCase "CR 723.6 a controlled player concedes themselves; their controller cannot do it for them" $
        -- alice controls bob (Mindslaver). Every ChooseAction for bob is answered
        -- by alice. The concede ask is NOT: it reaches bob, and bob takes it.
        -- If Prompt.Concede carried a Decider, this would be alice's call.
        --
        -- The premise -- that alice genuinely IS bob's decider for this run -- is
        -- not merely set up, it is OBSERVED: bob is given a land to play so a real
        -- Prompt.ChooseAction fires for him before he concedes, and the answerer
        -- records the Decider that prompt actually carried. A silent regression in
        -- Decide.deciderFor (activeControl stops being honoured) would make this
        -- record MkDecider bob instead, and the test would catch it even though
        -- the headline outcome (bob departs Conceded, alice wins) would still hold.
        let (mountainOid, gs) =
              S.addHandCard
                (Cards.mountainPrinting cards)
                S.bob
                ( (Setup.emptyGame S.bothPlayers)
                    { GameState.phase = Phase.PrecombatMain,
                      GameState.activePlayer = S.bob,
                      GameState.activeControl = Just (Decider.MkDecider S.alice)
                    }
                )
            -- (deciders seen for bob's ChooseAction, PlayerIds seen for Concede).
            -- Bob's first Concede ask answers Continues so the ChooseAction below
            -- fires and gets recorded; he plays the land (which keeps priority
            -- with him, CR 305.3 style timing aside -- Engine.priorityLoop simply
            -- re-loops with `priority = Just p` after a Play), and only on the
            -- SECOND Concede ask -- now with a recorded ChooseAction in hand --
            -- does he actually concede.
            answer :: Prompt.Prompt r -> State.State ([Decider.Decider], [PlayerId.PlayerId]) r
            answer p = case p of
              Prompt.ChooseAction decider pid _ ->
                if pid == S.bob
                  then do
                    State.modify' (\(ds, cs) -> (ds ++ [decider], cs))
                    pure (A.Play mountainOid)
                  else pure (S.identityAnswer p)
              Prompt.Concede asked -> do
                (_, asksSoFar) <- State.get
                State.modify' (\(ds, cs) -> (ds, cs ++ [asked]))
                pure (if null asksSoFar then Concession.Continues else Concession.Concedes)
              _ -> pure (S.identityAnswer p)
            ((_, after), (deciders, concedeAsks)) = State.runState (Engine.runGame answer gs Engine.runStep) ([], [])
         in do
              HU.assertEqual "bob's ChooseAction carried alice as decider (bob genuinely is controlled)" [Decider.MkDecider S.alice] deciders
              HU.assertEqual "every Concede ask reached bob himself, not his controller" [S.bob, S.bob] concedeAsks
              HU.assertEqual "bob left by his own concession" (Just (Status.Departed Departure.Conceded)) (fmap Player.status (Map.lookup S.bob (GameState.players after)))
              HU.assertEqual "alice wins" (Just (Result.Won S.alice)) (GameState.result after),
      HU.testCase "CR 104.3a concede does not use the stack: a spell on it never resolves" $
        -- A Lightning Bolt is on the stack targeting nothing in particular. alice
        -- concedes at her priority; the game ends without the stack resolving.
        let (spellId, base) = S.spellOnStack (Cards.lightningBoltPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            gs =
              base
                { GameState.phase = Phase.PrecombatMain,
                  GameState.activePlayer = S.alice
                }
            after = S.runPure (concedeAnswer S.alice) gs Engine.runStep
         in do
              HU.assertEqual "bob wins" (Just (Result.Won S.bob)) (GameState.result after)
              HU.assertEqual "the spell never left the stack" [spellId] (GameState.stack after)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Game"
    [gameTests cards, actionTests cards, objectFactTests cards, engineTests cards, ruleTests cards, restartReentryTests cards, concedeTests cards]

-- One Lightning Bolt in bob's hand.
handBobBolt :: Cards.Cards -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handBobBolt cards gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = S.bob,
            Object.source = Source.OfCard (Cards.lightningBoltPrinting cards),
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in (oid, gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2), GameState.hand = Map.insert S.bob (Seq.singleton oid) (GameState.hand gs2)})

namedIs :: Text.Text -> Maybe Object.Object -> Bool
namedIs wanted mo = case mo of
  Just o -> case Object.source o of
    Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
    Source.OfToken card -> Card.Type.name card == wanted
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
    Source.OfEmblem _ -> False
    Source.OfInherentTrigger _ _ -> False
  Nothing -> False

-- The controller's strategy: when asked to decide for bob (the CONTROLLED player,
-- routed because the prompt's Decider is alice), cast the Bolt at bob; otherwise
-- pass. A naive engine that ignored control would send the prompt with Decider =
-- bob, this interpreter would pass, and bob's life would stay 20 -- the falsifier.
slaveAnswer :: Prompt.Prompt r -> r
slaveAnswer p = case p of
  Prompt.ChooseAction (Decider.MkDecider d) player actions ->
    if player == S.bob && d == S.alice
      then case filter isCastAction actions of
        h : _ -> h
        [] -> A.Pass
      else A.Pass
  Prompt.ChooseTargets _ _ _ sets ->
    Map.mapMaybe
      (\s -> if Set.member (Recipient.ToPlayer S.bob) s then Just (Recipient.ToPlayer S.bob) else Set.lookupMin s)
      sets
  Prompt.Shuffle ids -> ids
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing
  Prompt.CastWhileSearching {} -> Nothing
  Prompt.ChooseX {} -> 0
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (take (fromIntegral count) (Set.toAscList legal))
  Prompt.ChooseCopyTarget {} -> Nothing
  Prompt.ChooseEntryOption {} -> 0
  Prompt.OrderTriggers _ _ sources -> map fromIntegral (take (length sources) [0 :: Int ..])
  Prompt.ChooseReplacement {} -> 0
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (take (fromIntegral count) candidates)
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> take (fromIntegral count) hand

-- CR 723.5 combat: alice, controlling bob, declares bob's attackers. Attackers
-- are declared only when the prompt's Decider is alice for player bob; a naive
-- engine that sent the prompt with Decider = bob would fall to `[]` and no one
-- would attack. Damage from the lone unblocked attacker goes to its sole
-- recipient (the defending player, alice). Everything else delegates to
-- slaveAnswer (blocks: none; priority: pass).
controlCombatAnswer :: Prompt.Prompt r -> r
controlCombatAnswer p = case p of
  Prompt.DeclareAttackers (Decider.MkDecider d) player attackers ->
    if d == S.alice && player == S.bob then attackers else []
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case Map.keys thresholds of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  _ -> slaveAnswer p

isCastAction :: A.Action -> Bool
isCastAction a = case a of
  A.Cast _ -> True
  _ -> False

-- Is this a legal-action Activate? On the gate board (a Mindslaver plus basic
-- lands, whose mana abilities are intrinsic and never surface as activated
-- abilities) the ONLY Activate action is Mindslaver's, so "the first Activate"
-- is unambiguously Mindslaver's control ability.
isActivateAction :: A.Action -> Bool
isActivateAction a = case a of
  A.Activate _ _ -> True
  _ -> False

-- CR 723 gate strategy. Alice, deciding for herself, fires Mindslaver (the only
-- activation on the board) at bob; once she is bob's decider (CR 723.5, the
-- prompt's Decider is alice while player is bob) she casts bob's Bolt; otherwise
-- pass. Non-ChooseAction prompts (targets, modes, shuffle, ...) delegate to
-- slaveAnswer, which targets bob. A naive engine ignoring control would send
-- bob's ChooseAction with Decider = bob; the else-branch would pass, bob would
-- keep 20 life, and the gate would fail -- the falsifier.
gateAnswer :: Prompt.Prompt r -> r
gateAnswer p = case p of
  Prompt.ChooseAction (Decider.MkDecider d) player actions ->
    case filter isActivateAction actions of
      activation : _ -> activation
      [] ->
        if player == S.bob && d == S.alice
          then case filter isCastAction actions of
            h : _ -> h
            [] -> A.Pass
          else A.Pass
  _ -> slaveAnswer p

-- CR 727 gate strategy. Whoever has priority activates the only activation on the
-- board -- the synthetic restart artifact (bob controls it) -- and otherwise
-- passes. Once the artifact is sacrificed as a cost there is no further
-- activation, so this fires exactly once; after the restart the artifact is in a
-- library, so no player can activate anything and everyone passes to termination.
-- Non-ChooseAction prompts (Shuffle during the rebuild, etc.) delegate to
-- identityAnswer.
restartAnswer :: Prompt.Prompt r -> r
restartAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isActivateAction actions of
      activation : _ -> activation
      [] -> A.Pass
  _ -> S.identityAnswer p

-- CR 729 gate strategy. Whoever has priority casts the only castable spell on the
-- board -- the synthetic subgame sorcery ({0}, in alice's hand) -- and otherwise
-- passes. Inside the subgame the libraries are Mountains (lands are PLAYED, not
-- cast), so no cast is available there and everyone passes to termination (bob
-- decks) -- except the CR 729.6 nested gate, where the level-1 subgame's
-- library also holds a castable nested synthetic-subgame sorcery, and the same
-- cast-if-available strategy descends into it. Because subgame prompts are
-- UNTAGGED, the same answerer serves every level. Non-ChooseAction prompts
-- (Shuffle during setup, etc.) delegate to identityAnswer.
subgameAnswer :: Prompt.Prompt r -> r
subgameAnswer p = case p of
  Prompt.ChooseAction _ _ actions ->
    case filter isCastAction actions of
      cast : _ -> cast
      [] -> A.Pass
  _ -> S.identityAnswer p

-- #136 / CR 729.2: hands the subgame's first-player roll a fixed answer, so a
-- test can play the same fixture with each player starting. Every other prompt
-- delegates to identityAnswer (which would answer this one with the head of the
-- order -- the pre-#136 behaviour).
firstPlayerAnswer :: PlayerId.PlayerId -> Prompt.Prompt r -> r
firstPlayerAnswer starter p = case p of
  Prompt.RandomFirstPlayer _ -> starter
  _ -> S.identityAnswer p

addManyG :: Cards.Cards -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addManyG cards n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid g)) gs (replicate n ())

-- Put one printing into a player's library as a fresh object; return its id.
-- Mirrors S.addHandCard, then relocates hand -> library.
libraryCard :: Printing.Printing -> PlayerId.PlayerId -> GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
libraryCard printing pid gs =
  let (oid, gs1) = S.addHandCard printing pid gs
      onLibrary o = o {Object.zone = Zone.Library}
   in ( oid,
        gs1
          { GameState.objects = Map.adjust onLibrary oid (GameState.objects gs1),
            GameState.hand = Map.adjust (Seq.filter (/= oid)) pid (GameState.hand gs1),
            GameState.library = Map.insertWith (flip (Seq.><)) pid (Seq.singleton oid) (GameState.library gs1)
          }
      )

-- Append n Mountains to a player's library.
addToLibraryG :: Cards.Cards -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
addToLibraryG cards n pid gs =
  List.foldl' (\g _ -> snd (libraryCard (Cards.mountainPrinting cards) pid g)) gs (replicate n ())

-- Move every object this player owns onto their library (mirror of
-- SetupSpec.poolToLibrary, adapted to GameSpec's imports): used to craft a
-- pre-shuffled library of a known size for a subgame/restart gate.
poolToLibraryG :: PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
poolToLibraryG pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

-- #134 / CR 727.4. A restart that resolves inside a LIVE Engine.runStep replaces
-- the game underneath the frames that are still running: the resolution, the
-- priority loop, and the step itself. Setup.restartGame leaves the rebuilt state
-- positioned just before turn 1's untap step with no player holding priority, so
-- every one of those frames has to unwind without touching it. The two things
-- that must NOT happen are a further priority grant ("No player has priority")
-- and Engine.advance, which would pop the FRESH `remaining` and skip turn 1's
-- untap step entirely.

-- A player's `n` owned cards, so the rebuilt game can deal a 7-card opening hand
-- without tripping the CR 727.3 short-deck loss.
ownedCards :: Cards.Cards -> Int -> PlayerId.PlayerId -> GameState.GameState -> GameState.GameState
ownedCards cards n pid gs =
  List.foldl' (\g _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid g)) gs [1 .. n]

-- alice is active in her precombat main phase (a step that grants priority) with
-- bob's RestartGame ability already on the stack. Both players pass, the ability
-- resolves, and the restart fires mid-step.
restartOnStack :: Cards.Cards -> GameState.GameState
restartOnStack cards =
  let g0 = Setup.emptyGame S.bothPlayers
      g1 = ownedCards cards 10 S.alice g0
      g2 = ownedCards cards 10 S.bob g1
      (abilId, g3) = Game.freshObjectId g2
      (ts, g4) = Game.freshTimestamp g3
      ability =
        ActivatedAbility.MkActivatedAbility
          { ActivatedAbility.cost =
              Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
            ActivatedAbility.modal =
              Modal.MkModal
                (Seq.singleton (Mode.MkMode (Seq.singleton Effect.RestartGame) Map.empty))
                (ModeSelection.ChooseExactly 1)
          }
      abilObj =
        Object.MkObject
          { Object.owner = S.bob,
            Object.source = Source.OfAbility (ObjectId.MkObjectId 0) ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing (Set.singleton (ModeIndex.MkModeIndex 0)),
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
   in g4
        { GameState.objects = Map.insert abilId abilObj (GameState.objects g4),
          GameState.stack = abilId : GameState.stack g4,
          GameState.activePlayer = S.alice,
          GameState.phase = Phase.PrecombatMain,
          GameState.remaining = Seq.fromList [Phase.PostcombatMain, Phase.Ending EndingStep.EndStep, Phase.Ending EndingStep.Cleanup]
        }

-- Run under a prompt-counting interpreter: how many times a player was asked to
-- act is exactly how CR 727.4's "No player has priority" is observed.
runCountingActions :: GameState.GameState -> Game.Type.Game a -> (GameState.GameState, Int)
runCountingActions gs act =
  let answer :: Prompt.Prompt r -> State.State Int r
      answer p = do
        case p of
          Prompt.ChooseAction {} -> State.modify' (+ 1)
          _ -> pure ()
        pure (S.identityAnswer p)
      ((_, gs1), n) = State.runState (Engine.runGame answer gs act) 0
   in (gs1, n)

restartReentryTests :: Cards.Cards -> Tasty.TestTree
restartReentryTests cards =
  Tasty.testGroup
    "restart re-entry (CR 727.4)"
    [ HU.testCase "the step the restart fired in does not advance past turn 1's untap step" $
        let (after, _) = runCountingActions (restartOnStack cards) Engine.runStep
         in do
              HU.assertEqual "still positioned at the untap step" Turn.firstPhase (GameState.phase after)
              HU.assertEqual "the fresh turn schedule is intact, not popped" Turn.laterPhases (GameState.remaining after)
              HU.assertEqual "turn 1 of the new game" 1 (GameState.turnNumber after)
              HU.assertEqual "the new game is still being played" Nothing (GameState.result after),
      HU.testCase "no player receives priority after the restart resolves" $
        -- alice passes, bob passes, the ability resolves: two ChooseAction prompts
        -- and no more. A third means the priority loop kept running on a game that
        -- no longer exists.
        let (_, asked) = runCountingActions (restartOnStack cards) Engine.runStep
         in HU.assertEqual "exactly the two passes that resolved the ability" 2 asked,
      HU.testCase "the next step runs the rebuilt turn 1's untap step" $
        let (afterRestart, _) = runCountingActions (restartOnStack cards) Engine.runStep
            (afterUntap, _) = runCountingActions afterRestart Engine.runStep
         in do
              HU.assertEqual "the untap step ran and handed on to upkeep" (Phase.Beginning BeginningStep.Upkeep) (GameState.phase afterUntap)
              HU.assertEqual "still turn 1" 1 (GameState.turnNumber afterUntap),
      HU.testCase "a live playGame survives the restart and plays the new game to a result" $
        -- An end-to-end liveness guard, not a discriminating one: the loop did
        -- not wedge before the fix either, it just played a turn 1 with no untap
        -- step. The three tests above are what actually catch that. This one
        -- pins the surrounding claim -- playGame keeps looping across the
        -- rebuild and returns the REBUILT game's result (CR 727.1: no player
        -- wins, loses or draws the game that was restarted). Terminating: the
        -- restart is a hand-built stack object, not a card in any library, so it
        -- cannot fire again and the rebuilt game decks out like any other.
        let (result, _) = Engine.runGamePure S.identityAnswer (restartOnStack cards) Engine.playGame
         in HU.assertBool "the new game reached a result" (case result of Result.Won _ -> True; Result.Drawn -> True)
    ]
