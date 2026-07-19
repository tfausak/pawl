{-# LANGUAGE GADTs #-}

-- Covers Pawl.Game, Pawl.Engine, and Pawl.Action: zones and changeZone, legal
-- actions, object facts, engine steps, and engine-rule integration (priority
-- rounds, the CR 103.7a draw skip, CR 514.2 discard, CR 704.5b deck-out).
module Pawl.GameSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Card as Card
import qualified Pawl.Cast as Cast
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.Decider as Decider
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
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

objectFactTests :: Tasty.TestTree
objectFactTests =
  Tasty.testGroup
    "ObjectFacts"
    [ HU.testCase "a Piker's power and toughness are 2 and 1" $
        let (oid, gs) = S.addPiker S.alice (Setup.emptyGame S.bothPlayers)
         in do
              HU.assertEqual "power" (Just 2) (Projection.powerOf oid gs)
              HU.assertEqual "toughness" (Just 1) (Projection.toughnessOf oid gs),
      HU.testCase "a Mountain has no power or toughness" $
        let gs = S.mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield S.alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> do
                HU.assertEqual "power" Nothing (Projection.powerOf oid gs)
                HU.assertEqual "toughness" Nothing (Projection.toughnessOf oid gs),
      HU.testCase "controllerOf is the owner while nothing can change control" $
        let (oid, gs) = S.addPiker S.bob (Setup.emptyGame S.bothPlayers)
         in HU.assertEqual "controller" (Just S.bob) (Game.controllerOf oid gs),
      HU.testCase "an unknown id has no facts" $
        let gs = Setup.emptyGame S.bothPlayers
            missing = ObjectId.MkObjectId 999
         in do
              HU.assertEqual "power" Nothing (Projection.powerOf missing gs)
              HU.assertEqual "controller" Nothing (Game.controllerOf missing gs)
    ]

gameTests :: Tasty.TestTree
gameTests =
  let after = Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield (S.oneMountainState Phase.PrecombatMain)
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
                      Object.source = Source.OfCard Card.mountainPrinting,
                      Object.zone = Zone.Battlefield,
                      Object.tapped = TapState.Untapped,
                      Object.damage = 0,
                      Object.sickness = Sickness.Sick,
                      Object.targets = Map.empty,
                      Object.chosenSubtypes = Map.empty,
                      -- changeZone draws a fresh timestamp; oneMountainState's
                      -- nextTimestamp starts at 1 (object 0 already holds 0).
                      Object.timestamp = Timestamp.MkTimestamp 1
                    }
              )
              (Game.lookupObject (ObjectId.MkObjectId 1) after),
          HU.testCase "CR 400.7 changeZone forgets a spell's targets" $
            let base = S.oneMountainState Phase.PrecombatMain
                stamped =
                  base
                    { GameState.objects =
                        Map.adjust
                          (\o -> o {Object.targets = Map.singleton (SlotName.MkSlotName (Text.pack "target")) (Recipient.ToPlayer S.alice)})
                          (ObjectId.MkObjectId 0)
                          (GameState.objects base)
                    }
                moved = Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield stamped
                landed = Map.elems (Map.filter (\o -> Object.zone o == Zone.Battlefield) (GameState.objects moved))
             in HU.assertEqual "reset" [Map.empty] (map Object.targets landed),
          HU.testCase "CR 400.7 changeZone resets chosenSubtypes" $
            let base = S.oneMountainState Phase.PrecombatMain
                slot = SlotName.MkSlotName (Text.pack "target")
                stamped =
                  base
                    { GameState.objects =
                        Map.adjust
                          (\o -> o {Object.chosenSubtypes = Map.singleton slot (Subtype.Mountain, Subtype.Island)})
                          (ObjectId.MkObjectId 0)
                          (GameState.objects base)
                    }
                moved = Event.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield stamped
                newObj = Game.lookupObject (ObjectId.MkObjectId 1) moved
             in HU.assertEqual "reset to empty" (Just Map.empty) (fmap Object.chosenSubtypes newObj),
          HU.testCase "CR 613.7d changeZone stamps the new incarnation with a fresh timestamp" $
            let (oid, gs) = S.addPiker S.bob (S.mountainsInPlay 1)
                before = GameState.nextTimestamp gs
                movedState = Event.changeZone oid Zone.Graveyard gs
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
            HU.assertEqual "empty" [] (Card.Type.staticAbilities S.pikerCard)
        ]

actionTests :: Tasty.TestTree
actionTests =
  Tasty.testGroup
    "Action"
    [ HU.testCase "a land in hand is playable in a main phase" $
        HU.assertBool "play" (A.Play (ObjectId.MkObjectId 0) `elem` Action.legalActions S.alice (S.oneMountainState Phase.PrecombatMain)),
      HU.testCase "passing is always legal" $
        HU.assertBool "pass" (A.Pass `elem` Action.legalActions S.alice (S.oneMountainState Phase.PrecombatMain)),
      HU.testCase "no land play outside a main phase" $
        HU.assertEqual "only pass" [A.Pass] (Action.legalActions S.alice (S.oneMountainState (Phase.Beginning BeginningStep.Upkeep))),
      HU.testCase "no second land after one is played" $
        let gs = (S.oneMountainState Phase.PrecombatMain) {GameState.landPlayed = Set.singleton S.alice}
         in HU.assertEqual "only pass" [A.Pass] (Action.legalActions S.alice gs)
    ]

goldfishResult :: (Result.Result, GameState.GameState)
goldfishResult =
  Engine.runMatchPure S.identityAnswer S.redRed

landState :: GameState.GameState
landState =
  snd (Engine.runGamePure S.playLandAnswer (Setup.emptyGame S.bothPlayers) (Engine.playFrom S.redRed))

-- Alice is active on turns 1, 3, 5, …; bob on 2, 4, 6, …. With one land play per
-- turn (CR 305.2) a player can never have more lands out than turns taken.
turnsTaken :: PlayerId.PlayerId -> GameState.GameState -> Int
turnsTaken pid gs =
  let total = fromIntegral (GameState.turnNumber gs)
   in if pid == S.alice then (total + 1) `div` 2 else total `div` 2

engineTests :: Tasty.TestTree
engineTests =
  Tasty.testGroup
    "Engine"
    [ HU.testCase "goldfish game ends with the starting player winning" $
        HU.assertEqual "winner" (Result.Won S.alice) (fst goldfishResult),
      HU.testCase "card conservation holds at end" $
        HU.assertEqual "objects" 120 (Game.objectCount (snd goldfishResult)),
      HU.testCase "playing lands fills the battlefield" $
        HU.assertBool "non-empty" $
          not (null (Game.zoneMembers Zone.Battlefield S.alice landState)),
      HU.testCase "land play conserves cards" $
        HU.assertEqual "objects" 120 (Game.objectCount landState),
      HU.testCase "CR 305.2 at most one land per turn" $
        HU.assertBool "no double land plays" $
          length (Game.zoneMembers Zone.Battlefield S.alice landState) <= turnsTaken S.alice landState
            && length (Game.zoneMembers Zone.Battlefield S.bob landState) <= turnsTaken S.bob landState
    ]

-- Run setup, then a scripted tweak, then whatever steps the scenario needs.
scenario :: Game.Type.Game () -> GameState.GameState
scenario steps =
  snd $ Engine.runGamePure S.identityAnswer (Setup.emptyGame (NonEmpty.map fst S.redRed)) $ do
    Setup.newGame S.redRed
    steps

-- Alice starts, so her turn-1 draw is skipped.
aliceFirstDraw :: GameState.GameState
aliceFirstDraw = scenario S.drawStep

-- Bob is not the starting player, so his draw happens normally.
bobFirstDraw :: GameState.GameState
bobFirstDraw = scenario $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
  S.drawStep

bobAfterCleanup :: GameState.GameState
bobAfterCleanup = scenario $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = S.bob, GameState.turnNumber = 2}
  S.drawStep
  Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)

deckedOut :: GameState.GameState
deckedOut = scenario $ do
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
  Prompt.DeclareAttackers {} -> pure []
  Prompt.DeclareBlockers {} -> pure Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    pure $ case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.Shuffle ids -> pure ids
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

-- pikerInHand already builds on Setup.emptyGame bothPlayers, so turnOrder is
-- [alice, bob] and both players are in the players map.
askedPlayers :: [PlayerId.PlayerId]
askedPlayers =
  let (gs, _) = S.pikerInHand 3 Phase.PrecombatMain
   in State.execState
        (Program.foldProgramM recordingAnswer (State.runStateT Engine.priorityLoop gs))
        []

ruleTests :: Tasty.TestTree
ruleTests =
  Tasty.testGroup
    "Rules"
    [ HU.testCase "CR 117.4 a full round of passes resolves the stack, not the step" $
        -- With a spell on the stack, everyone passing must RESOLVE it and keep
        -- the step alive. Under M0's rule the step would simply end with the
        -- spell still sitting on the stack.
        let (gs, oid) = S.pikerInHand 3 Phase.PrecombatMain
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
        HU.assertEqual "alice twice, then bob" [S.alice, S.alice, S.bob] (take 3 askedPlayers),
      HU.testCase "CR 103.7a starting player skips first draw" $ do
        HU.assertEqual "hand" 7 (S.handSize S.alice aliceFirstDraw)
        HU.assertEqual "library" 53 (librarySize S.alice aliceFirstDraw),
      HU.testCase "CR 103.7a only the starting player skips" $ do
        HU.assertEqual "hand" 8 (S.handSize S.bob bobFirstDraw)
        HU.assertEqual "library" 52 (librarySize S.bob bobFirstDraw),
      HU.testCase "CR 514.2 discard to hand size" $
        HU.assertEqual "hand" 7 (S.handSize S.bob bobAfterCleanup),
      HU.testCase "CR 704.5b deck-out loses" $
        HU.assertEqual
          "alice departed"
          (Just (Status.Departed Departure.Lost))
          (fmap Player.status (Map.lookup S.alice (GameState.players deckedOut))),
      HU.testCase "CR 704.5b the survivor wins" $
        HU.assertEqual "bob won" (Just (Result.Won S.bob)) (GameState.result deckedOut),
      HU.testCase "CR 723.3/723.5: alice decides for bob, but bob's resources move" $
        -- bob's main phase, controlled by alice, with a Mountain and a Bolt.
        let g0 = Setup.emptyGame S.bothPlayers
            (_mtnId, g1) = S.addCreature Card.mountainPrinting S.bob g0
            (_boltId, g2) = handBobBolt g1
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
              HU.assertEqual "the Mountain (bob's) is tapped" 1 (S.tappedCount S.bob after)
    ]

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Game"
    [gameTests, actionTests, objectFactTests, engineTests, ruleTests]

-- One Lightning Bolt in bob's hand.
handBobBolt :: GameState.GameState -> (ObjectId.ObjectId, GameState.GameState)
handBobBolt gs =
  let (oid, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = S.bob,
            Object.source = Source.OfCard Card.lightningBoltPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.targets = Map.empty,
            Object.chosenSubtypes = Map.empty,
            Object.timestamp = ts
          }
   in (oid, gs2 {GameState.objects = Map.insert oid obj (GameState.objects gs2), GameState.hand = Map.insert S.bob (Seq.singleton oid) (GameState.hand gs2)})

namedIs :: Text.Text -> Maybe Object.Object -> Bool
namedIs wanted mo = case mo of
  Just o -> case Object.source o of
    Source.OfCard printing -> Card.Type.name (Printing.card printing) == wanted
    Source.OfAbility _ _ -> False
    Source.OfTrigger _ _ -> False
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
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    case filter S.isCreatureRecipient (Map.keys thresholds) of
      r : _ -> Map.singleton r n
      [] -> Map.empty
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  Prompt.SearchLibrary {} -> Nothing

isCastAction :: A.Action -> Bool
isCastAction a = case a of
  A.Cast _ -> True
  _ -> False
