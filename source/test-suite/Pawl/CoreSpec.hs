{-# LANGUAGE GADTs #-}

-- Covers the VM core: Pawl.Type.Program (the suspension interpreter) and
-- Pawl.Quantity (numeric evaluation).
module Pawl.CoreSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.Program as Program
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

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

-- A GameState holding one object whose binding environment carries the given
-- chosen X (Nothing = no amount bound), for exercising Quantity.evaluate's X arm.
withBoundAmount :: Cards.Cards -> Maybe Integer -> (ObjectId.ObjectId, GameState.GameState)
withBoundAmount cards mAmount =
  let (oid, gs0) = S.addCreature (Cards.mountainPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
      bindings = Binding.fromChoices Map.empty Map.empty (fmap fromInteger mAmount)
      gs = gs0 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) oid (GameState.objects gs0)}
   in (oid, gs)

quantityTests :: Cards.Cards -> Tasty.TestTree
quantityTests cards =
  Tasty.testGroup
    "Quantity"
    [ HU.testCase "a literal evaluates to itself" $
        HU.assertEqual
          "literal"
          (Just 2)
          (Quantity.evaluate (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal 2)),
      HU.testCase "a literal may be negative" $
        HU.assertEqual
          "negative"
          (Just (-1))
          (Quantity.evaluate (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal (-1))),
      HU.testCase "evaluate reads X from the object's binding environment" $
        let (oid, gs) = withBoundAmount cards (Just 5)
         in HU.assertEqual "X = 5" (Just 5) (Quantity.evaluate gs oid Quantity.Type.X),
      HU.testCase "evaluate X is Nothing when no amount was bound" $
        let (oid, gs) = withBoundAmount cards Nothing
         in HU.assertEqual "unbound X" Nothing (Quantity.evaluate gs oid Quantity.Type.X)
    ]

tests :: Cards.Cards -> Tasty.TestTree
tests cards = Tasty.testGroup "Core" [programTests, quantityTests cards]
