module Pawl.Codec.CostReductionSpec where

import qualified Pawl.Codec.CostReduction as CostReduction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.CostReduction as CostReduction
import qualified Pawl.Types.Count as Count
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.ManaSymbol as ManaSymbol
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.Scope as Scope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CostReduction" $ do
  -- CR 601.2f, as Thrasta, Tempest's Roar costs {3} less for each other spell
  -- cast this turn.
  Spec.it s "MkCostReduction" $
    Common.assertCodec
      s
      CostReduction.codec
      ( CostReduction.MkCostReduction
          { CostReduction.amount = ManaCost.MkManaCost [ManaSymbol.Generic 3],
            CostReduction.perEach =
              Quantity.Count
                Count.MkCount
                  { Count.scope = Scope.InHistory EventShape.SpellCast,
                    Count.filter = Filter.And [],
                    Count.aggregation = Aggregation.Members
                  },
            CostReduction.condition = Nothing
          }
      )
      " {\"amount\":[{\"type\":\"Generic\",\"value\":3}],\"perEach\":{\"type\":\"Count\",\"value\":{\"scope\":{\"type\":\"InHistory\",\"value\":{\"type\":\"SpellCast\"}},\"filter\":{\"type\":\"And\",\"value\":[]},\"aggregation\":{\"type\":\"Members\"}}}} "
  -- CR 601.2f, as Ertai's Scorn costs {U} less if an opponent cast two or more
  -- spells this turn.
  Spec.it s "with a condition" $
    Common.assertCodec
      s
      CostReduction.codec
      ( CostReduction.MkCostReduction
          { CostReduction.amount = ManaCost.MkManaCost [ManaSymbol.Generic 1],
            CostReduction.perEach = Quantity.Literal 1,
            CostReduction.condition = Just (Condition.Compares (Compares.MkCompares (Quantity.Literal 2) Comparison.AtLeast (Quantity.Literal 3)))
          }
      )
      " {\"amount\":[{\"type\":\"Generic\",\"value\":1}],\"perEach\":{\"type\":\"Literal\",\"value\":1},\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"Literal\",\"value\":2},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":3}}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CostReduction.codec
