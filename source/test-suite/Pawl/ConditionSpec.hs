-- Covers Pawl.Condition, Pawl.Type.Condition and Pawl.Type.Comparison.
module Pawl.ConditionSpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Condition as Condition
import qualified Pawl.Count as Count
import qualified Pawl.Filter as Filter
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Aggregation as Aggregation
import qualified Pawl.Type.Comparison as Comparison
import qualified Pawl.Type.Condition as Condition.Type
import qualified Pawl.Type.Count as Count.Type
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ObjectId as ObjectId
import qualified Pawl.Type.PlayerRef as PlayerRef
import qualified Pawl.Type.PlayerRelation as PlayerRelation
import qualified Pawl.Type.Quantity as Quantity.Type
import qualified Pawl.Type.Scope as Scope
import qualified Pawl.Type.Subtype as Subtype
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Count every battlefield object; the stub view decides how many match.
everyPermanent :: Count.Type.Count
everyPermanent =
  Count.Type.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.Type.And [])
    Aggregation.Objects

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  let gs0 = Setup.emptyGame S.bothPlayers
      -- n Swamps on the battlefield, and a ViewOf (via S.stubView, Pawl.Support's
      -- second consumer per Task 3) describing each of them.
      boardOf :: Integer -> (Count.ViewOf, GameState.GameState)
      boardOf n =
        let step (ids, g) _ =
              let (oid, g') = S.addCreature (Cards.swampPrinting cards) S.alice g
               in (ids <> [oid], g')
            (oids, gs) = foldl step ([], gs0) [1 .. n]
            table = fmap (\oid -> (oid, Set.empty, Set.singleton Subtype.Swamp, Just S.alice)) oids
         in (S.stubView table, gs)
      context = Filter.MkContext (Just S.alice) (Just (ObjectId.MkObjectId 0))
      check n comparison threshold =
        let (viewOf, gs) = boardOf n
         in Condition.holds
              viewOf
              context
              gs
              (ObjectId.MkObjectId 0)
              (Condition.Type.MkCondition everyPermanent comparison (Quantity.Type.Literal threshold))
   in Tasty.testGroup
        "Condition"
        [ HU.testCase "CR 603.8 Exactly holds only on equality" $ do
            HU.assertBool "0 == 0" (check 0 Comparison.Exactly 0)
            HU.assertBool "1 /= 0" (not (check 1 Comparison.Exactly 0)),
          HU.testCase "AtLeast holds at and above the threshold" $ do
            HU.assertBool "3 >= 3" (check 3 Comparison.AtLeast 3)
            HU.assertBool "2 < 3" (not (check 2 Comparison.AtLeast 3)),
          HU.testCase "AtMost holds at and below the threshold" $ do
            HU.assertBool "0 <= 1" (check 0 Comparison.AtMost 1)
            HU.assertBool "1 <= 1" (check 1 Comparison.AtMost 1)
            HU.assertBool "2 > 1" (not (check 2 Comparison.AtMost 1)),
          HU.testCase "an undeterminable COUNT is false, never true" $
            -- Relative with no perspective: Count.evaluate is Nothing, and a
            -- total holds must collapse that to False (CR 611.2b's conservative
            -- reading), not to a vacuous True.
            let (viewOf, gs) = boardOf 0
                count =
                  Count.Type.MkCount
                    (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
                    (Filter.Type.And [])
                    Aggregation.Objects
             in HU.assertBool "false" . not $
                  Condition.holds
                    viewOf
                    (Filter.MkContext Nothing Nothing)
                    gs
                    (ObjectId.MkObjectId 0)
                    (Condition.Type.MkCondition count Comparison.Exactly (Quantity.Type.Literal 0)),
          HU.testCase "an undeterminable THRESHOLD is false, never true" $
            -- Quantity.X with no binding on the object: same collapse.
            let (viewOf, gs) = boardOf 0
             in HU.assertBool "false" . not $
                  Condition.holds
                    viewOf
                    context
                    gs
                    (ObjectId.MkObjectId 0)
                    (Condition.Type.MkCondition everyPermanent Comparison.Exactly Quantity.Type.X)
        ]
