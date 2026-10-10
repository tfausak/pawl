{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CostReduction where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.CostDirection as CostDirection
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CostDirection as CostDirection.Type
import qualified Pawl.Types.CostReduction as CostReduction

-- | A bare object keyed by the record's field names, the shape
-- Pawl.Codec.AppliedReduction takes.
--
-- 'amount' and 'perEach' are REQUIRED: Ertai's Scorn's fixed reduction spells
-- its Literal 1 'perEach' out rather than leaning on a default. An absent
-- 'condition' is Nothing, the unconditional reduction Thrasta prints; an absent
-- 'whichTargets' names no target, and an absent 'direction' is Less, the
-- sentence every card but a "costs more" one prints.
codec :: Codec.Codec CostReduction.CostReduction
codec = Fields.object $ do
  amount <- Fields.required "amount" ManaCost.codec CostReduction.amount
  perEach <- Fields.required "perEach" Quantity.codec CostReduction.perEach
  condition <- Fields.defaulted "condition" Nothing (Common.maybe Condition.codec) CostReduction.condition
  whichTargets <- Fields.defaulted "whichTargets" Nothing (Common.maybe (Filter.codec Keyword.codec)) CostReduction.whichTargets
  direction <- Fields.defaulted "direction" CostDirection.Type.Less CostDirection.codec CostReduction.direction
  pure
    CostReduction.MkCostReduction
      { CostReduction.amount = amount,
        CostReduction.perEach = perEach,
        CostReduction.condition = condition,
        CostReduction.whichTargets = whichTargets,
        CostReduction.direction = direction
      }
