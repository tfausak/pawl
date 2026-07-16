{-# LANGUAGE GADTs #-}

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Card as Card
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Replay as Replay
import qualified Pawl.Sba as Sba
import qualified Pawl.Setup as Setup
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Color as Color
import qualified Pawl.Type.Departure as Departure
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Mana as Mana.Type
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.ManaType as ManaType
import qualified Pawl.Type.ManaUnit as ManaUnit
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.Player as Player
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Power as Power
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Result as Result
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Status as Status
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.Toughness as Toughness
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified System.Random as Random
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "pawl"
    [ programTests,
      cardTests,
      turnTests,
      gameTests,
      actionTests,
      setupTests,
      sbaTests,
      engineTests,
      replayTests,
      propertyTests,
      ruleTests,
      quantityTests,
      manaTests
    ]

-- alice controls n untapped Mountains on the battlefield, nothing else.
mountainsInPlay :: Int -> GameState.GameState
mountainsInPlay n =
  let add gs _ =
        let (oid, gs1) = Game.freshObjectId gs
            obj =
              Object.MkObject
                { Object.owner = alice,
                  Object.source = Source.OfCard Card.mountainPrinting,
                  Object.zone = Zone.Battlefield,
                  Object.tapped = TapState.Untapped
                }
         in gs1
              { GameState.objects = Map.insert oid obj (GameState.objects gs1),
                GameState.battlefield = Set.insert oid (GameState.battlefield gs1)
              }
   in List.foldl' add (Setup.emptyGame bothPlayers) [1 .. n]

pikerCost :: ManaCost.ManaCost
pikerCost = ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]

poolSize :: PlayerId.PlayerId -> GameState.GameState -> Int
poolSize pid gs = case Mana.poolOf pid gs of
  Mana.Type.MkMana units -> length units

tappedCount :: PlayerId.PlayerId -> GameState.GameState -> Int
tappedCount pid gs =
  let isTapped oid = case Game.lookupObject oid gs of
        Just obj -> Object.tapped obj == TapState.Tapped
        Nothing -> False
   in length (filter isTapped (Game.zoneMembers Zone.Battlefield pid gs))

manaTests :: Tasty.TestTree
manaTests =
  Tasty.testGroup
    "Mana"
    [ HU.testCase "CR 305.6 a Mountain's red mana ability comes from its subtype" $
        HU.assertEqual
          "red"
          (Just (ManaType.Colored Color.Red))
          (Mana.subtypeMana Subtype.Mountain),
      HU.testCase "a Goblin grants no mana ability" $
        HU.assertEqual "none" Nothing (Mana.subtypeMana Subtype.Goblin),
      HU.testCase "an empty pool starts empty" $
        HU.assertEqual "empty" 0 (poolSize alice (mountainsInPlay 2)),
      HU.testCase "tapping a Mountain taps it and adds one red unit" $
        let gs = mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ -> do
                let after = Mana.tapForMana oid gs
                HU.assertEqual "tapped" 1 (tappedCount alice after)
                HU.assertEqual
                  "pool"
                  (Mana.Type.MkMana [ManaUnit.MkManaUnit {ManaUnit.manaType = ManaType.Colored Color.Red}])
                  (Mana.poolOf alice after),
      HU.testCase "two Mountains can pay {1}{R}" $
        HU.assertBool "affordable" (Mana.canPay alice pikerCost (mountainsInPlay 2)),
      HU.testCase "one Mountain cannot pay {1}{R}" $
        HU.assertBool "unaffordable" (not (Mana.canPay alice pikerCost (mountainsInPlay 1))),
      HU.testCase "no Mountains cannot pay {1}{R}" $
        HU.assertBool "unaffordable" (not (Mana.canPay alice pikerCost (mountainsInPlay 0))),
      HU.testCase "paying {1}{R} taps exactly two of three Mountains and leaves no float" $
        case Mana.payCost alice pikerCost (mountainsInPlay 3) of
          Nothing -> HU.assertFailure "three Mountains should pay {1}{R}"
          Just after -> do
            HU.assertEqual "tapped" 2 (tappedCount alice after)
            HU.assertEqual "no float" 0 (poolSize alice after),
      HU.testCase "CR 500.4 mana pools empty" $
        let gs = mountainsInPlay 1
         in case Game.zoneMembers Zone.Battlefield alice gs of
              [] -> HU.assertFailure "fixture should have one Mountain"
              oid : _ ->
                HU.assertEqual "emptied" 0 (poolSize alice (Mana.emptyManaPools (Mana.tapForMana oid gs)))
    ]

quantityTests :: Tasty.TestTree
quantityTests =
  Tasty.testGroup
    "Quantity"
    [ HU.testCase "a literal evaluates to itself" $
        HU.assertEqual
          "literal"
          (Just 2)
          (Quantity.evaluate (Setup.emptyGame bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal 2)),
      HU.testCase "a literal may be negative" $
        HU.assertEqual
          "negative"
          (Just (-1))
          (Quantity.evaluate (Setup.emptyGame bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal (-1)))
    ]

alice, bob :: PlayerId.PlayerId
alice = PlayerId.MkPlayerId 0
bob = PlayerId.MkPlayerId 1

bothPlayers :: NonEmpty.NonEmpty PlayerId.PlayerId
bothPlayers = alice NonEmpty.:| [bob]

-- A GameState with a single Mountain in alice's hand, in a chosen phase.
oneMountainState :: Phase.Phase -> GameState.GameState
oneMountainState ph =
  let oid = ObjectId.MkObjectId 0
      obj =
        Object.MkObject
          { Object.owner = alice,
            Object.source = Source.OfCard Card.mountainPrinting,
            Object.zone = Zone.Hand,
            Object.tapped = TapState.Untapped
          }
   in GameState.MkGameState
        { GameState.objects = Map.singleton oid obj,
          GameState.library = Map.empty,
          GameState.hand = Map.singleton alice (Seq.singleton oid),
          GameState.graveyard = Map.empty,
          GameState.battlefield = mempty,
          GameState.exile = mempty,
          GameState.stack = [],
          GameState.players = Map.empty,
          GameState.manaPool = Map.empty,
          GameState.turnOrder = [alice],
          GameState.activePlayer = alice,
          GameState.phase = ph,
          GameState.priority = Just alice,
          GameState.passes = 0,
          GameState.turnNumber = 1,
          GameState.result = Nothing,
          GameState.nextObjectId = ObjectId.MkObjectId 1,
          GameState.drewFromEmpty = mempty,
          GameState.landPlayed = mempty
        }

gameTests :: Tasty.TestTree
gameTests =
  let after = Game.changeZone (ObjectId.MkObjectId 0) Zone.Battlefield (oneMountainState Phase.PrecombatMain)
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
                    { Object.owner = alice,
                      Object.source = Source.OfCard Card.mountainPrinting,
                      Object.zone = Zone.Battlefield,
                      Object.tapped = TapState.Untapped
                    }
              )
              (Game.lookupObject (ObjectId.MkObjectId 1) after)
        ]

actionTests :: Tasty.TestTree
actionTests =
  Tasty.testGroup
    "Action"
    [ HU.testCase "a land in hand is playable in a main phase" $
        HU.assertBool "play" (A.Play (ObjectId.MkObjectId 0) `elem` Action.legalActions alice (oneMountainState Phase.PrecombatMain)),
      HU.testCase "passing is always legal" $
        HU.assertBool "pass" (A.Pass `elem` Action.legalActions alice (oneMountainState Phase.PrecombatMain)),
      HU.testCase "no land play outside a main phase" $
        HU.assertEqual "only pass" [A.Pass] (Action.legalActions alice (oneMountainState (Phase.Beginning BeginningStep.Upkeep))),
      HU.testCase "no second land after one is played" $
        let gs = (oneMountainState Phase.PrecombatMain) {GameState.landPlayed = Set.singleton alice}
         in HU.assertEqual "only pass" [A.Pass] (Action.legalActions alice gs)
    ]

-- Identity interpreter: shuffle returns ids unchanged; actions never occur here.
identityAnswer :: Prompt.Prompt r -> r
identityAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction {} -> A.Pass

setupState :: GameState.GameState
setupState =
  Program.foldProgram
    identityAnswer
    (State.execStateT (Setup.newGame bothPlayers) (Setup.emptyGame bothPlayers))

setupTests :: Tasty.TestTree
setupTests =
  Tasty.testGroup
    "Setup"
    [ HU.testCase "120 objects after setup" $
        HU.assertEqual "count" 120 (Game.objectCount setupState),
      HU.testCase "each library has 53 after opening draws" $
        HU.assertEqual "library" 53 (length (Game.zoneMembers Zone.Library alice setupState)),
      HU.testCase "each hand has 7" $
        HU.assertEqual "hand" 7 (length (Game.zoneMembers Zone.Hand bob setupState)),
      HU.testCase "active player is first in turn order" $
        HU.assertEqual "active" alice (GameState.activePlayer setupState)
    ]

sbaBase :: GameState.GameState
sbaBase = Setup.emptyGame bothPlayers

sbaTests :: Tasty.TestTree
sbaTests =
  Tasty.testGroup
    "Sba"
    [ HU.testCase "drew-from-empty loses" $
        let after = Sba.checkStateBasedActions sbaBase {GameState.drewFromEmpty = Set.singleton alice}
         in HU.assertEqual "alice lost" (Just (Status.Departed Departure.Lost)) (fmap Player.status (Map.lookup alice (GameState.players after))),
      HU.testCase "one remaining player wins" $
        let after = Sba.checkStateBasedActions sbaBase {GameState.drewFromEmpty = Set.singleton alice}
         in HU.assertEqual "bob won" (Just (Result.Won bob)) (GameState.result after),
      HU.testCase "life <= 0 loses" $
        let gs = sbaBase {GameState.players = Map.insert alice (Player.MkPlayer {Player.life = 0, Player.status = Status.Playing}) (GameState.players sbaBase)}
         in HU.assertEqual "bob won" (Just (Result.Won bob)) (GameState.result (Sba.checkStateBasedActions gs)),
      HU.testCase "simultaneous last departures draw" $
        let after = Sba.checkStateBasedActions sbaBase {GameState.drewFromEmpty = Set.fromList [alice, bob]}
         in HU.assertEqual "draw" (Just Result.Drawn) (GameState.result after)
    ]

goldfishResult :: (Result.Result, GameState.GameState)
goldfishResult =
  Engine.runGamePure identityAnswer (Setup.emptyGame bothPlayers) (Engine.playFrom bothPlayers)

-- Always plays a land when one is legal, otherwise passes.
playLandAnswer :: Prompt.Prompt r -> r
playLandAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction _ _ actions ->
    let isPlay a = case a of
          A.Play _ -> True
          A.Pass -> False
     in case filter isPlay actions of
          h : _ -> h
          [] -> A.Pass

landState :: GameState.GameState
landState =
  snd (Engine.runGamePure playLandAnswer (Setup.emptyGame bothPlayers) (Engine.playFrom bothPlayers))

-- Alice is active on turns 1, 3, 5, …; bob on 2, 4, 6, …. With one land play per
-- turn (CR 305.2) a player can never have more lands out than turns taken.
turnsTaken :: PlayerId.PlayerId -> GameState.GameState -> Int
turnsTaken pid gs =
  let total = fromIntegral (GameState.turnNumber gs)
   in if pid == alice then (total + 1) `div` 2 else total `div` 2

engineTests :: Tasty.TestTree
engineTests =
  Tasty.testGroup
    "Engine"
    [ HU.testCase "goldfish game ends with the starting player winning" $
        HU.assertEqual "winner" (Result.Won alice) (fst goldfishResult),
      HU.testCase "card conservation holds at end" $
        HU.assertEqual "objects" 120 (Game.objectCount (snd goldfishResult)),
      HU.testCase "playing lands fills the battlefield" $
        HU.assertBool "non-empty" $
          not (null (Game.zoneMembers Zone.Battlefield alice landState)),
      HU.testCase "land play conserves cards" $
        HU.assertEqual "objects" 120 (Game.objectCount landState),
      HU.testCase "CR 305.2 at most one land per turn" $
        HU.assertBool "no double land plays" $
          length (Game.zoneMembers Zone.Battlefield alice landState) <= turnsTaken alice landState
            && length (Game.zoneMembers Zone.Battlefield bob landState) <= turnsTaken bob landState
    ]

replayTests :: Tasty.TestTree
replayTests =
  let start = Setup.emptyGame bothPlayers
      game = Engine.playFrom bothPlayers
      -- Recorded with playLandAnswer, whose choices differ from Replay's
      -- exhausted-transcript fallback. That keeps these assertions honest: the
      -- transcript has to actually carry the decisions.
      ((_, recorded), transcript) = Replay.record playLandAnswer start game
   in Tasty.testGroup
        "Replay"
        [ HU.testCase "replaying a recorded game reproduces the final state" $
            HU.assertEqual "final states equal" recorded (snd (Replay.replay transcript start game)),
          HU.testCase "the transcript is what carries the decisions" $
            HU.assertBool "empty log diverges" $
              recorded /= snd (Replay.replay [] start game),
          HU.testCase "a recorded goldfish also replays" $
            let ((_, gf), gfLog) = Replay.record identityAnswer start game
             in HU.assertEqual "goldfish" gf (snd (Replay.replay gfLog start game))
        ]

-- A StdGen-driven interpreter: random shuffle and random legal action.
randomAnswer :: Prompt.Prompt r -> State.State Random.StdGen r
randomAnswer p = case p of
  Prompt.Shuffle ids -> do
    g <- State.get
    let (g1, g2) = Random.splitGen g
    State.put g2
    pure (shuffleWith g1 ids)
  Prompt.ChooseAction _ _ actions -> do
    g <- State.get
    let n = length actions
        (i, g') = Random.uniformR (0, max 0 (n - 1)) g
    State.put g'
    pure (pick actions (min (n - 1) (max 0 i)))

-- Total index into a list; the engine always offers at least Pass, so the
-- fallback is unreachable in practice but keeps this free of partial functions.
pick :: [A.Action] -> Int -> A.Action
pick actions i = case drop i actions of
  h : _ -> h
  [] -> A.Pass

shuffleWith :: Random.StdGen -> [a] -> [a]
shuffleWith g xs =
  let unfoldInts :: Random.StdGen -> [Int]
      unfoldInts gen = let (v, gen') = Random.uniform gen in v : unfoldInts gen'
      insertByKey y ys = case ys of
        [] -> [y]
        z : zs -> if fst y <= fst z then y : z : zs else z : insertByKey y zs
      keys = take (length xs) (unfoldInts g)
   in map snd (foldr insertByKey [] (zip keys xs))

runRandomGame :: Int -> GameState.GameState
runRandomGame s =
  let start = Setup.emptyGame bothPlayers
      game = Engine.playFrom bothPlayers
      (_, final) = State.evalState (Program.foldProgramM randomAnswer (State.runStateT game start)) (Random.mkStdGen s)
   in final

nextIdOf :: GameState.GameState -> Integer
nextIdOf gs = case GameState.nextObjectId gs of
  ObjectId.MkObjectId n -> toInteger n

propertyTests :: Tasty.TestTree
propertyTests =
  Tasty.testGroup
    "Properties"
    [ QC.testProperty "conservation: 120 objects at end" $ \s ->
        Game.objectCount (runRandomGame s) QC.=== 120,
      QC.testProperty "every game terminates with a result" $ \s ->
        QC.property (Maybe.isJust (GameState.result (runRandomGame s))),
      QC.testProperty "at least 120 ids were minted" $ \s ->
        QC.property (nextIdOf (runRandomGame s) >= 120),
      QC.testProperty "no life changes in M0" $ \s ->
        QC.property (all (\pl -> Player.life pl == Setup.startingLife) (Map.elems (GameState.players (runRandomGame s))))
    ]

-- Run setup, then a scripted tweak, then whatever steps the scenario needs.
scenario :: Game.Type.Game () -> GameState.GameState
scenario steps =
  snd $ Engine.runGamePure identityAnswer (Setup.emptyGame bothPlayers) $ do
    Setup.newGame bothPlayers
    steps

drawStep :: Game.Type.Game ()
drawStep = Engine.runTurnBasedActions (Phase.Beginning BeginningStep.DrawStep)

-- Alice starts, so her turn-1 draw is skipped.
aliceFirstDraw :: GameState.GameState
aliceFirstDraw = scenario drawStep

-- Bob is not the starting player, so his draw happens normally.
bobFirstDraw :: GameState.GameState
bobFirstDraw = scenario $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = bob, GameState.turnNumber = 2}
  drawStep

bobAfterCleanup :: GameState.GameState
bobAfterCleanup = scenario $ do
  State.modify' $ \gs -> gs {GameState.activePlayer = bob, GameState.turnNumber = 2}
  drawStep
  Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)

deckedOut :: GameState.GameState
deckedOut = scenario $ do
  State.modify' $ \gs ->
    gs
      { GameState.library = Map.insert alice Seq.empty (GameState.library gs),
        GameState.turnNumber = 3
      }
  drawStep
  Engine.checkSba

handSize :: PlayerId.PlayerId -> GameState.GameState -> Int
handSize pid gs = length (Game.zoneMembers Zone.Hand pid gs)

librarySize :: PlayerId.PlayerId -> GameState.GameState -> Int
librarySize pid gs = length (Game.zoneMembers Zone.Library pid gs)

ruleTests :: Tasty.TestTree
ruleTests =
  Tasty.testGroup
    "Rules"
    [ HU.testCase "CR 103.7a starting player skips first draw" $ do
        HU.assertEqual "hand" 7 (handSize alice aliceFirstDraw)
        HU.assertEqual "library" 53 (librarySize alice aliceFirstDraw),
      HU.testCase "CR 103.7a only the starting player skips" $ do
        HU.assertEqual "hand" 8 (handSize bob bobFirstDraw)
        HU.assertEqual "library" 52 (librarySize bob bobFirstDraw),
      HU.testCase "CR 514.2 discard to hand size" $
        HU.assertEqual "hand" 7 (handSize bob bobAfterCleanup),
      HU.testCase "CR 704.5b deck-out loses" $
        HU.assertEqual
          "alice departed"
          (Just (Status.Departed Departure.Lost))
          (fmap Player.status (Map.lookup alice (GameState.players deckedOut))),
      HU.testCase "CR 704.5b the survivor wins" $
        HU.assertEqual "bob won" (Just (Result.Won bob)) (GameState.result deckedOut)
    ]

-- A toy instruction set for exercising Program.
data Toy r where
  Ask :: Toy Int

toyProgram :: Program.Program Toy Int
toyProgram = do
  x <- Program.prompt Ask
  y <- Program.prompt Ask
  pure (x + y)

programTests :: Tasty.TestTree
programTests =
  Tasty.testGroup
    "Program"
    [ HU.testCase "pure interpreter threads answers" $
        let answer :: Toy b -> b
            answer i = case i of Ask -> 21
         in HU.assertEqual "21 + 21" 42 (Program.foldProgram answer toyProgram),
      HU.testCase "effectful interpreter runs in order" $
        let answer :: Toy b -> State.State [Int] b
            answer i = case i of
              Ask -> do
                xs <- State.get
                case xs of
                  h : t -> do State.put t; pure h
                  [] -> pure 0
         in HU.assertEqual "1 + 2" 3 (State.evalState (Program.foldProgramM answer toyProgram) [1, 2])
    ]

pikerCard :: Card.Type.Card
pikerCard = Printing.card Card.pikerPrinting

cardTests :: Tasty.TestTree
cardTests =
  Tasty.testGroup
    "Card"
    [ HU.testCase "Mountain printing is named Mountain" $
        HU.assertEqual "name" (Text.pack "Mountain") (Card.Type.name (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain is a Land" $
        HU.assertBool "isLand" (Card.isLand (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain has the Mountain subtype" $
        HU.assertBool "subtype" $
          Set.member Subtype.Mountain (TypeLine.subtypes (Card.Type.typeLine (Printing.card Card.mountainPrinting))),
      HU.testCase "Mountain type line contains Land" $
        HU.assertBool "cardtype" $
          Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card Card.mountainPrinting))),
      -- CR 202.1: a land has no mana cost. Not a zero cost -- no cost at all.
      HU.testCase "Mountain has no mana cost" $
        HU.assertEqual "no cost" Nothing (Card.Type.manaCost (Printing.card Card.mountainPrinting)),
      HU.testCase "Mountain has no power or toughness" $ do
        HU.assertEqual "power" Nothing (Card.Type.power (Printing.card Card.mountainPrinting))
        HU.assertEqual "toughness" Nothing (Card.Type.toughness (Printing.card Card.mountainPrinting)),
      HU.testCase "Piker printing is named Goblin Piker" $
        HU.assertEqual "name" (Text.pack "Goblin Piker") (Card.Type.name pikerCard),
      HU.testCase "Piker costs {1}{R}" $
        HU.assertEqual
          "cost"
          (Just (ManaCost.MkManaCost [ManaSymbol.Generic 1, ManaSymbol.OfType (ManaType.Colored Color.Red)]))
          (Card.Type.manaCost pikerCard),
      HU.testCase "Piker is a 2/1" $ do
        HU.assertEqual "power" (Just (Power.MkPower (Quantity.Type.Literal 2))) (Card.Type.power pikerCard)
        HU.assertEqual "toughness" (Just (Toughness.MkToughness (Quantity.Type.Literal 1))) (Card.Type.toughness pikerCard),
      HU.testCase "Piker is a Goblin Warrior" $
        HU.assertEqual
          "subtypes"
          (Set.fromList [Subtype.Goblin, Subtype.Warrior])
          (TypeLine.subtypes (Card.Type.typeLine pikerCard)),
      HU.testCase "Piker is a creature and not a land" $ do
        HU.assertBool "creature" (Card.isCreature pikerCard)
        HU.assertBool "not land" (not (Card.isLand pikerCard)),
      -- CR 110.1: the classification resolution turns on. Never card identity.
      HU.testCase "CR 110.1 both a Piker and a Mountain are permanents" $ do
        HU.assertBool "piker" (Card.isPermanent pikerCard)
        HU.assertBool "mountain" (Card.isPermanent (Printing.card Card.mountainPrinting))
    ]

turnSequence :: [Phase.Phase]
turnSequence = go Turn.firstPhase
  where
    go p = p : maybe [] go (Turn.next p)

turnTests :: Tasty.TestTree
turnTests =
  Tasty.testGroup
    "Turn"
    [ HU.testCase "firstPhase is the untap step" $
        HU.assertEqual "firstPhase" (Phase.Beginning BeginningStep.Untap) Turn.firstPhase,
      HU.testCase "a turn has twelve steps in order" $
        HU.assertEqual "sequence" Turn.allPhases turnSequence,
      HU.testCase "next returns Nothing after cleanup" $
        HU.assertEqual "end" Nothing (Turn.next (Phase.Ending EndingStep.Cleanup)),
      HU.testCase "untap and cleanup grant no priority" $
        HU.assertBool "no priority" $
          not (Turn.grantsPriority (Phase.Beginning BeginningStep.Untap))
            && not (Turn.grantsPriority (Phase.Ending EndingStep.Cleanup)),
      QC.testProperty "next never revisits a phase in a turn" $
        QC.property (length turnSequence == length (dedupe turnSequence))
    ]

dedupe :: (Eq a) => [a] -> [a]
dedupe xs = case xs of
  [] -> []
  h : t -> h : dedupe (filter (/= h) t)
