{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CostReduction where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CostReduction as CostReduction

-- | A bare object keyed by the record's field names, the shape
-- Pawl.Codec.ReduceSpellCost takes.
--
-- 'amount' and 'perEach' are REQUIRED: Ertai's Scorn's fixed reduction spells
-- its Literal 1 'perEach' out rather than leaning on a default. An absent
-- 'condition' is Nothing, the unconditional reduction Thrasta prints.
codec :: Codec.Codec CostReduction.CostReduction
codec = Fields.object $ do
  amount <- Fields.required "amount" ManaCost.codec CostReduction.amount
  perEach <- Fields.required "perEach" Quantity.codec CostReduction.perEach
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) CostReduction.condition
  pure
    CostReduction.MkCostReduction
      { CostReduction.amount = amount,
        CostReduction.perEach = perEach,
        CostReduction.condition = condition
      }
