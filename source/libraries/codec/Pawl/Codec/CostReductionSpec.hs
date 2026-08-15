{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CostReductionSpec where

import qualified Pawl.Codec.CostReduction as CostReduction
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Aggregation as Aggregation
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
                  }
          }
      )
      """ {"amount":[{"type":"Generic","value":3}],"perEach":{"type":"Count","value":{"scope":{"type":"InHistory","value":{"type":"SpellCast"}},"filter":{"type":"And","value":[]},"aggregation":{"type":"Members"}}}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s CostReduction.codec
