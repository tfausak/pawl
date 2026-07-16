{-# LANGUAGE GADTs #-}

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Card as Card
import qualified Pawl.Game as Game
import qualified Pawl.Turn as Turn
import qualified Pawl.Type.BeginningStep as BeginningStep
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.EndingStep as EndingStep
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Phase as Phase
import qualified Pawl.Type.PlayerId as PlayerId
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Source as Source
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TypeLine as TypeLine
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "pawl" [programTests, cardTests, turnTests, gameTests]

alice :: PlayerId.PlayerId
alice = PlayerId.MkPlayerId 0

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
          Set.member CardType.Land (TypeLine.types (Card.Type.typeLine (Printing.card Card.mountainPrinting)))
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
