{-# LANGUAGE GADTs #-}

-- Covers the VM core: Pawl.Types.Program (the suspension interpreter) and
-- Pawl.Engine.Quantity (numeric evaluation).
module Pawl.CoreSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
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
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Zone as Zone

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

programSpec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()
programSpec s = Spec.describe s "Pawl.Types.Program" $ do
  Spec.it s "pure interpreter threads answers" $ do
    let answer :: Toy b -> b
        answer i = case i of Ask -> 21
    Spec.assertEq s (Program.foldProgram answer toyProgram) 42

  Spec.it s "effectful interpreter runs in order" $ do
    let answer :: Toy b -> State.State [Int] b
        answer i = case i of
          Ask -> do
            xs <- State.get
            case xs of
              h : t -> do State.put t; pure h
              [] -> pure 0
    Spec.assertEq s (State.evalState (Program.foldProgramM answer toyProgram) [1, 2]) 3

-- A GameState holding one object whose binding environment carries the given
-- chosen X (Nothing = no amount bound), for exercising Quantity.evaluate's X arm.
withBoundAmount :: Printing.Printing -> Maybe Natural.Natural -> (ObjectId.ObjectId, GameState.GameState)
withBoundAmount mountain mAmount =
  let (oid, gs0) = S.addCreature mountain S.alice (Setup.emptyGame S.bothPlayers)
      bindings = Binding.fromChoices Map.empty mAmount Set.empty
      gs = gs0 {GameState.objects = Map.adjust (\o -> o {Object.bindings = bindings}) oid (GameState.objects gs0)}
   in (oid, gs)

-- No view is ever consulted by a non-Count arm, so a trivial ViewOf/Context
-- pair is the honest fixture for those tests.
noView :: ObjectId.ObjectId -> Maybe Filter.View
noView _ = Nothing

noContext :: Filter.Context
noContext = Filter.MkContext Nothing Nothing

quantitySpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
quantitySpec s registry = Spec.describe s "Pawl.Engine.Quantity" $ do
  Spec.it s "a literal evaluates to itself" $ do
    Spec.assertEq s (Quantity.evaluate noView noContext (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal 2)) $ Just 2

  Spec.it s "a literal may be negative" $ do
    Spec.assertEq s (Quantity.evaluate noView noContext (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) (Quantity.Type.Literal (-1))) $ Just (-1)

  Spec.it s "evaluate reads X from the object's binding environment" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs) = withBoundAmount mountain (Just 5)
    Spec.assertEq s (Quantity.evaluate noView noContext gs oid Quantity.Type.X) $ Just 5

  Spec.it s "evaluate X is Nothing when no amount was bound" $ do
    mountain <- S.printingOf s registry "Mountain"
    let (oid, gs) = withBoundAmount mountain Nothing
    Spec.assertEq s (Quantity.evaluate noView noContext gs oid Quantity.Type.X) Nothing

  Spec.it s "CR 208.2 Star alone is not evaluable -- it is notation, resolved at the seed" $ do
    Spec.assertEq s (Quantity.evaluate noView noContext (Setup.emptyGame S.bothPlayers) (ObjectId.MkObjectId 0) Quantity.Type.Star) Nothing

  Spec.it s "CR 208.2 Plus adds, so 1+* composes without a new case" $ do
    Spec.assertEq
      s
      ( Quantity.evaluate
          noView
          noContext
          (Setup.emptyGame S.bothPlayers)
          (ObjectId.MkObjectId 0)
          (Quantity.Type.Plus (Quantity.Type.Literal 1) (Quantity.Type.Literal 2))
      )
      $ Just 3

  Spec.it s "Plus is Nothing when either side is unevaluable" $ do
    Spec.assertEq
      s
      ( Quantity.evaluate
          noView
          noContext
          (Setup.emptyGame S.bothPlayers)
          (ObjectId.MkObjectId 0)
          (Quantity.Type.Plus (Quantity.Type.Literal 1) Quantity.Type.Star)
      )
      Nothing

  Spec.it s "substituteStar replaces Star everywhere, including inside Plus" $ do
    Spec.assertEq
      s
      ( Quantity.substituteStar
          (Quantity.Type.Literal 7)
          (Quantity.Type.Plus (Quantity.Type.Literal 1) Quantity.Type.Star)
      )
      $ Quantity.Type.Plus (Quantity.Type.Literal 1) (Quantity.Type.Literal 7)

  Spec.it s "Count CardsInYourHand is Nothing with no 'you'" $ do
    let gs = Setup.emptyGame S.bothPlayers
        viewOf = Projection.fullView gs
    Spec.assertEq
      s
      ( Quantity.evaluate
          viewOf
          (Filter.MkContext Nothing Nothing)
          gs
          (ObjectId.MkObjectId 0)
          (Quantity.Type.Count cardsInYourHand)
      )
      Nothing

  Spec.it s "Count CardsInYourHand counts that player's hand" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    let (gs, _) = S.handOne piker (Setup.emptyGame S.bothPlayers)
        viewOf = Projection.fullView gs
    Spec.assertEq s (Quantity.evaluate viewOf (Filter.MkContext (Just S.alice) Nothing) gs (ObjectId.MkObjectId 0) (Quantity.Type.Count cardsInYourHand)) $ Just 1

  Spec.it s "Count CardTypesInAllGraveyards counts DISTINCT card types, not cards" $ do
    piker <- S.printingOf s registry "Goblin Piker"
    warMammoth <- S.printingOf s registry "War Mammoth"
    lightningBolt <- S.printingOf s registry "Lightning Bolt"
    let gs0 = Setup.emptyGame S.bothPlayers
        (_, one) = S.addGraveyardCard piker S.alice gs0
        (_, two) = S.addGraveyardCard warMammoth S.bob one
        (_, three) = S.addGraveyardCard lightningBolt S.alice two
        viewOfTwo = Projection.fullView two
        viewOfThree = Projection.fullView three
    Spec.assertEqWith
      s
      "two creatures in two graveyards is one type"
      (Quantity.evaluate viewOfTwo noContext two (ObjectId.MkObjectId 0) (Quantity.Type.Count cardTypesInAllGraveyards))
      $ Just 1
    Spec.assertEqWith
      s
      "adding an instant makes two"
      (Quantity.evaluate viewOfThree noContext three (ObjectId.MkObjectId 0) (Quantity.Type.Count cardTypesInAllGraveyards))
      $ Just 2

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = do
  programSpec s
  quantitySpec s registry
