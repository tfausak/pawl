{-# LANGUAGE GADTs #-}

-- Covers the VM core: Pawl.Type.Program (the suspension interpreter) and
-- Pawl.Quantity (numeric evaluation).
module Pawl.CoreSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
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

quantityTests :: Tasty.TestTree
quantityTests =
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
          (Quantity.evaluate (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal (-1)))
    ]

tests :: Tasty.TestTree
tests = Tasty.testGroup "Core" [programTests, quantityTests]
