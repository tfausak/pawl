{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.CostReduction where

import qualified Pawl.Codec.ManaCost as ManaCost
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CostReduction as CostReduction

-- | A bare object keyed by the record's field names, the shape
-- Pawl.Codec.ReduceSpellCost takes.
--
-- Both keys are REQUIRED. A fixed self-reduction would be a Literal 1 'perEach'
-- and could default to one, but no card in the pool prints that sentence, so
-- the default would be untested wire spelling.
codec :: Codec.Codec CostReduction.CostReduction
codec = Fields.object $ do
  amount <- Fields.required "amount" ManaCost.codec CostReduction.amount
  perEach <- Fields.required "perEach" Quantity.codec CostReduction.perEach
  pure
    CostReduction.MkCostReduction
      { CostReduction.amount = amount,
        CostReduction.perEach = perEach
      }
