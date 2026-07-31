-- Covers Pawl.Engine.Condition, Pawl.Types.Condition and Pawl.Types.Comparison.
module Pawl.ConditionSpec where

import qualified Data.Set as Set
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Support as S
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import qualified Pawl.Types.Count as Count.Type
import qualified Pawl.Types.Filter as Filter.Type
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Quantity as Quantity.Type
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Count every battlefield object; the stub view decides how many match.
everyPermanent :: Count.Type.Count Quantity.Type.Quantity
everyPermanent =
  Count.Type.MkCount
    (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
    (Filter.Type.And [])
    Aggregation.Objects

-- n Swamps on the battlefield, and a ViewOf (via S.stubView, Pawl.Support's
-- second consumer per Task 3) describing each of them.
boardOf :: Printing.Printing -> Integer -> (Count.ViewOf, GameState.GameState)
boardOf swamp n =
  let gs0 = Setup.emptyGame S.bothPlayers
      step (ids, g) _ =
        let (oid, g') = S.addCreature swamp S.alice g
         in (ids <> [oid], g')
      (oids, gs) = foldl step ([], gs0) [1 .. n]
      table = fmap (\oid -> (oid, Set.empty, Set.singleton Subtype.Swamp, Just S.alice)) oids
   in (S.stubView table, gs)

context :: Filter.Context
context = Filter.MkContext (Just S.alice) (Just (ObjectId.MkObjectId 0))

check :: Printing.Printing -> Integer -> Comparison.Comparison -> Integer -> Bool
check swamp n comparison threshold =
  let (viewOf, gs) = boardOf swamp n
   in Condition.holds
        viewOf
        context
        gs
        (ObjectId.MkObjectId 0)
        (Condition.Type.MkCondition (Quantity.Type.Count everyPermanent) comparison (Quantity.Type.Literal threshold))

tests :: Registry.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Condition"
    [ HU.testCase "CR 603.8 Exactly holds only on equality" $ do
        swamp <- Registry.printing registry "Swamp"
        HU.assertBool "0 == 0" (check swamp 0 Comparison.Exactly 0)
        HU.assertBool "1 /= 0" (not (check swamp 1 Comparison.Exactly 0)),
      HU.testCase "AtLeast holds at and above the threshold" $ do
        swamp <- Registry.printing registry "Swamp"
        HU.assertBool "3 >= 3" (check swamp 3 Comparison.AtLeast 3)
        HU.assertBool "2 < 3" (not (check swamp 2 Comparison.AtLeast 3)),
      HU.testCase "AtMost holds at and below the threshold" $ do
        swamp <- Registry.printing registry "Swamp"
        HU.assertBool "0 <= 1" (check swamp 0 Comparison.AtMost 1)
        HU.assertBool "1 <= 1" (check swamp 1 Comparison.AtMost 1)
        HU.assertBool "2 > 1" (not (check swamp 2 Comparison.AtMost 1)),
      HU.testCase "an undeterminable MEASURED side is false, never true" $ do
        -- Relative with no perspective: Count.evaluate is Nothing, and a
        -- total holds must collapse that to False (CR 611.2b's conservative
        -- reading), not to a vacuous True.
        swamp <- Registry.printing registry "Swamp"
        let (viewOf, gs) = boardOf swamp 0
            count =
              Count.Type.MkCount
                (Scope.InZone Zone.Hand (PlayerRef.Relative PlayerRelation.You))
                (Filter.Type.And [])
                Aggregation.Objects
        HU.assertBool "false" . not $
          Condition.holds
            viewOf
            (Filter.MkContext Nothing Nothing)
            gs
            (ObjectId.MkObjectId 0)
            (Condition.Type.MkCondition (Quantity.Type.Count count) Comparison.Exactly (Quantity.Type.Literal 0)),
      HU.testCase "an undeterminable THRESHOLD side is false, never true" $ do
        -- Quantity.X with no binding on the object: same collapse.
        swamp <- Registry.printing registry "Swamp"
        let (viewOf, gs) = boardOf swamp 0
        HU.assertBool "false" . not $
          Condition.holds
            viewOf
            context
            gs
            (ObjectId.MkObjectId 0)
            (Condition.Type.MkCondition (Quantity.Type.Count everyPermanent) Comparison.Exactly Quantity.Type.X)
    ]
