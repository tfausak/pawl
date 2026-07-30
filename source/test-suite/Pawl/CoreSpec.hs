{-# LANGUAGE GADTs #-}

-- Covers the VM core: Pawl.Types.Program (the suspension interpreter) and
-- Pawl.Quantity (numeric evaluation).
module Pawl.CoreSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Binding as Binding
import qualified Pawl.Filter as Filter
import qualified Pawl.Projection as Projection
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Registry as Registry.Type
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- CR 608.2h: the "you" player's hand (Inner Calm, Outer Strength's shape).
cardsInYourHand :: Count.Type.Count Quantity.Type.Quantity
cardsInYourHand =
  Count.Type.MkCount
    (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
    (Filter.Type.And [])
    Aggregation.Objects

-- CR 208.2a: the distinct card types among every graveyard (Tarmogoyf's shape).
cardTypesInAllGraveyards :: Count.Type.Count Quantity.Type.Quantity
cardTypesInAllGraveyards =
  Count.Type.MkCount
    (Scope.InZone Zone.Graveyard PlayerRef.EachPlayer)
    (Filter.Type.And [])
    Aggregation.DistinctCardTypes

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
withBoundAmount :: Printing.Printing -> Maybe Natural.Natural -> (ObjectId.ObjectId, GameState.GameState)
withBoundAmount mountain mAmount =
  let (oid, gs0) = S.addCreature mountain S.alice (Setup.emptyGame S.bothPlayers)
      bindings = Binding.fromChoices Map.empty Map.empty mAmount Set.empty
      gs = gs0 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) oid (GameState.objects gs0)}
   in (oid, gs)

-- No view is ever consulted by a non-Count arm, so a trivial ViewOf/Context
-- pair is the honest fixture for those tests.
noView :: ObjectId.ObjectId -> Maybe Filter.View
noView _ = Nothing

noContext :: Filter.Context
noContext = Filter.MkContext Nothing Nothing

quantityTests :: Registry.Type.Registry -> Tasty.TestTree
quantityTests registry =
  Tasty.testGroup
    "Quantity"
    [ HU.testCase "a literal evaluates to itself" $
        HU.assertEqual
          "literal"
          (Just 2)
          (Quantity.evaluate noView noContext (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal 2)),
      HU.testCase "a literal may be negative" $
        HU.assertEqual
          "negative"
          (Just (-1))
          (Quantity.evaluate noView noContext (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal (-1))),
      HU.testCase "evaluate reads X from the object's binding environment" $ do
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs) = withBoundAmount mountain (Just 5)
        HU.assertEqual "X = 5" (Just 5) (Quantity.evaluate noView noContext gs oid Quantity.Type.X),
      HU.testCase "evaluate X is Nothing when no amount was bound" $ do
        mountain <- Registry.printing registry "Mountain"
        let (oid, gs) = withBoundAmount mountain Nothing
        HU.assertEqual "unbound X" Nothing (Quantity.evaluate noView noContext gs oid Quantity.Type.X),
      HU.testCase "CR 208.2 Star alone is not evaluable -- it is notation, resolved at the seed" $
        HU.assertEqual
          "Star"
          Nothing
          (Quantity.evaluate noView noContext (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) Quantity.Type.Star),
      HU.testCase "CR 208.2 Plus adds, so 1+* composes without a new case" $
        HU.assertEqual
          "1 + 2"
          (Just 3)
          ( Quantity.evaluate
              noView
              noContext
              (Setup.emptyGame S.bothPlayers)
              (ObjectId.MkObjectId 0)
              (Quantity.Type.Plus (Quantity.Type.Literal 1) (Quantity.Type.Literal 2))
          ),
      HU.testCase "Plus is Nothing when either side is unevaluable" $
        HU.assertEqual
          "1 + Star"
          Nothing
          ( Quantity.evaluate
              noView
              noContext
              (Setup.emptyGame S.bothPlayers)
              (ObjectId.MkObjectId 0)
              (Quantity.Type.Plus (Quantity.Type.Literal 1) Quantity.Type.Star)
          ),
      HU.testCase "substituteStar replaces Star everywhere, including inside Plus" $
        HU.assertEqual
          "1 + Literal 7"
          (Quantity.Type.Plus (Quantity.Type.Literal 1) (Quantity.Type.Literal 7))
          ( Quantity.substituteStar
              (Quantity.Type.Literal 7)
              (Quantity.Type.Plus (Quantity.Type.Literal 1) Quantity.Type.Star)
          ),
      HU.testCase "Count CardsInYourHand is Nothing with no 'you'" $
        let gs = Setup.emptyGame S.bothPlayers
            viewOf = Projection.fullView gs
         in HU.assertEqual
              "no player"
              Nothing
              ( Quantity.evaluate
                  viewOf
                  (Filter.MkContext Nothing Nothing)
                  gs
                  (ObjectId.MkObjectId 0)
                  (Quantity.Type.Count cardsInYourHand)
              ),
      HU.testCase "Count CardsInYourHand counts that player's hand" $ do
        piker <- Registry.printing registry "Goblin Piker"
        let (gs, _) = S.handOne piker (Setup.emptyGame S.bothPlayers)
            viewOf = Projection.fullView gs
        HU.assertEqual
          "one card"
          (Just 1)
          (Quantity.evaluate viewOf (Filter.MkContext (Just S.alice) Nothing) gs (ObjectId.MkObjectId 0) (Quantity.Type.Count cardsInYourHand)),
      HU.testCase "Count CardTypesInAllGraveyards counts DISTINCT card types, not cards" $ do
        piker <- Registry.printing registry "Goblin Piker"
        warMammoth <- Registry.printing registry "War Mammoth"
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, one) = S.addGraveyardCard piker S.alice gs0
            (_, two) = S.addGraveyardCard warMammoth S.bob one
            (_, three) = S.addGraveyardCard lightningBolt S.alice two
            viewOfTwo = Projection.fullView two
            viewOfThree = Projection.fullView three
        HU.assertEqual
          "two creatures in two graveyards is one type"
          (Just 1)
          (Quantity.evaluate viewOfTwo noContext two (ObjectId.MkObjectId 0) (Quantity.Type.Count cardTypesInAllGraveyards))
        HU.assertEqual
          "adding an instant makes two"
          (Just 2)
          (Quantity.evaluate viewOfThree noContext three (ObjectId.MkObjectId 0) (Quantity.Type.Count cardTypesInAllGraveyards))
    ]

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry = Tasty.testGroup "Core" [programTests, quantityTests registry]
